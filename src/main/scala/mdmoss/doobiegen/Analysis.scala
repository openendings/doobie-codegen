package mdmoss.doobiegen

import mdmoss.doobiegen.Runner.Target
import mdmoss.doobiegen.sql.{Column, Table, TableRef}

case class RowRepField(source: List[Column], scalaName: String, scalaType: ScalaType)

case class Insert(fn: FunctionDef)

case class Create(fn: FunctionDef)

case class Get(inner: FunctionDef, outer: FunctionDef)

case class Find(inner: FunctionDef, outer: FunctionDef)

case class All(inner: FunctionDef, outer: FunctionDef)

case class AllUnbounded(inner: FunctionDef, outer: FunctionDef)

case class Count(inner: FunctionDef, outer: FunctionDef)

case class MultiGet(inner: FunctionDef, outer: FunctionDef)

object Analysis {

  /* Helpers */
  implicit class CamelCaseStrings(s: String) {
    def camelCase: String = """_([a-z])""".r.replaceAllIn(s, m => m.group(1).capitalize)
  }

  implicit class RowRepsForInsert(l: List[RowRepField]) {
    def sqlColumns: String = l.flatMap(_.source).map(_.sqlName).mkString(", ")
  }

  /** Returns an arbitrary using the given constructor and the arb instance for each type in order */
  def merge(constructor: String, scalaTypes: List[ScalaType]): String = {
    s"$constructor(" + scalaTypes.map(_.arb).mkString(", ") + ")"
  }

  def makeSafe(string: String): String = if (ReservedWords.contains(string)) s"`$string`" else string

  val ReservedWords = Seq(
    "type",
    "package"
  )

  val SkipInsert = Seq(
    sql.BigSerial
  )
}

class Analysis(val model: DbModel, val target: Target) {
  import Analysis._

  def targetPackage(table: Table) = target.`package` + table.ref.schema.map(s => s".$s").getOrElse("") + ".gen"

  def targetObject(table: Table) = table.ref.sqlName.camelCase.capitalize

  def privateScope(table: Table) = target.`package`.split('.').reverse.head

  def resolve(ref: TableRef): Table = model.tables.filter { t =>
    t.ref.schema.map(_.toLowerCase) == ref.schema.map(_.toLowerCase) &&
    t.ref.sqlName.toLowerCase == ref.sqlName.toLowerCase
  }.head


  def pkNewType(table: Table): Option[(List[RowRepField], ScalaType)] = table.primaryKeyColumns match {
    case Nil      => None
    case c :: Nil =>
      val rep = c.scalaRep.copy(scalaName = "value")
      val symbol = targetObject(table) + c.sqlName.camelCase.capitalize
      val fullyQualified = s"${targetPackage(table)}.${targetObject(table)}.$symbol"
      Some((List(rep), ScalaType(symbol, s"$fullyQualified(${c.scalaType.arb})")))
    case cs       => None
  }

  def rowNewType(table: Table): (List[RowRepField], ScalaType) = {
    /* We'll put the primary key first, if any, then other components */
    val pkPart = pkNewType(table).map {
      case (reps, newType) => reps match {
        case r :: Nil => RowRepField(r.source, r.source.head.scalaName, newType)
        case rs       => RowRepField(rs.flatMap(_.source), "pk", newType)
      }
    }

    val parts = pkPart.toList ++ table.nonPrimaryKeyColumns.map(_.scalaRep)
    (parts, ScalaType(targetObject(table) + "Row", merge(targetObject(table) + "Row", parts.map(_.scalaType))))
  }

  /* We only generate a Shape if there's a primary key, meaning we can't use Row */
  def rowShape(table: Table): Option[(List[RowRepField], ScalaType)] = pkNewType(table).map { pk =>
    val parts = table.nonPrimaryKeyColumns.map(_.scalaRep)
    (parts, ScalaType("Shape", merge(targetObject(table) + "Shape", parts.map(_.scalaType))))
  }

  def insert(table: Table): Insert = {
    val params = rowNewType(table)._1.filterNot(r => SkipInsert.contains(r.source.head.sqlType)).map(t => FunctionParam(t.scalaName, t.scalaType))
    val body =
      s"""sql\"\"\"
          |  INSERT INTO ${table.ref.fullName} (${rowNewType(table)._1.sqlColumns})
          |  VALUES (${params.map(_.name).map(s => s"$${$s}").mkString(", ")})
          |\"\"\".update
      """.stripMargin.trim

    val fn = FunctionDef(Some(privateScope(table)), "insert", params, "Update0", body)
    Insert(fn)
  }

  def create(table: Table): Create = {
    val in = insert(table)
    val params = in.fn.params.map(f => s"${f.name}: ${f.`type`.symbol}").mkString(", ")
    val rowType = rowNewType(table)

    val body =
      s"""
        |  insert(${in.fn.params.map(f => f.name).mkString(", ")})
        |    .withUniqueGeneratedKeys[${rowType._2.symbol}](${rowType._1.flatMap(_.source.map(s => "\"" + s.sqlName + "\"")).mkString(", ")})
     """.stripMargin

    val fn = FunctionDef(None, "create", in.fn.params, s"ConnectionIO[${rowType._2.symbol}]", body)
    Create(fn)
  }

  def get(table: Table): Option[Get] = pkNewType(table).map { pk =>

    val rowType = rowNewType(table)

    val innerBody =
      s"""sql\"\"\"
         |  SELECT ${rowType._1.sqlColumns}
         |  FROM ${table.ref.fullName}
         |  WHERE ${pk._1.sqlColumns} = $$id
         |\"\"\".query[${rowType._2.symbol}]
       """.stripMargin

    val inner = FunctionDef(Some(privateScope(table)), "getInner", Seq(FunctionParam("id", pk._2)), s"Query0[${rowType._2.symbol}]", innerBody)

    val outerBody =
      s"""
         |getInner(${inner.params.map(_.name).mkString(",")}).unique
       """.stripMargin

    val outer = FunctionDef(None, "get", inner.params, s"ConnectionIO[${rowType._2.symbol}]", outerBody)

    Get(inner, outer)
  }

  def find(table: Table): Option[Find] = pkNewType(table).map { pk =>

    val rowType = rowNewType(table)

    val innerBody =
      s"""sql\"\"\"
          |  SELECT ${rowType._1.sqlColumns}
          |  FROM ${table.ref.fullName}
          |  WHERE ${pk._1.sqlColumns} = $$id
          |\"\"\".query[${rowType._2.symbol}]
       """.stripMargin

    val params = pk._1 match {
      case p :: Nil => Seq(FunctionParam("id", p.scalaType))
      case ps => ps.zipWithIndex.map { case (p, i) => FunctionParam(s"id$i", p.scalaType)}
    }
    val inner = FunctionDef(Some(privateScope(table)), "findInner", params, s"Query0[${rowType._2.symbol}]", innerBody)

    val outerBody = s"""findInner(${params.map(_.name).mkString(", ")}).option"""
    val outer = FunctionDef(None, "find", params, s"ConnectionIO[Option[${rowType._2.symbol}]]", outerBody)

    Find(inner, outer)
  }

  def allUnbounded(table: Table): AllUnbounded = {
    val rowType = rowNewType(table)

    val innerBody =
      s"""sql\"\"\"
         |  SELECT ${rowType._1.sqlColumns}
         |  FROM ${table.ref.fullName}
         |\"\"\".query[${rowType._2.symbol}]
       """.stripMargin

    val inner = FunctionDef(Some(privateScope(table)), "allUnboundedInner", Seq(), s"Query0[${rowType._2.symbol}]", innerBody)

    val outerBody =
      s"""allUnboundedInner().list"""

    val outer = FunctionDef(None, "allUnbounded", Seq(), s"ConnectionIO[List[${rowType._2.symbol}]]", outerBody)

    AllUnbounded(inner, outer)
  }

  def all(table: Table): All = {
    val rowType = rowNewType(table)
    val offsetLimit = Seq(FunctionParam("offset", ScalaType("Long", "0L")), FunctionParam("limit", ScalaType("Long", "0L")))

    val innerBody =
      s"""sql\"\"\"
          |  SELECT ${rowType._1.sqlColumns}
          |  FROM ${table.ref.fullName}
          |  OFFSET $$offset
          |  LIMIT $$limit
          |\"\"\".query[${rowType._2.symbol}]
       """.stripMargin

    val inner = FunctionDef(Some(privateScope(table)), "allInner", offsetLimit, s"Query0[${rowType._2.symbol}]", innerBody)

    val outerBody =
      s"""allInner(offset, limit).list"""

    val outer = FunctionDef(None, "all", offsetLimit, s"ConnectionIO[List[${rowType._2.symbol}]]", outerBody)

    All(inner, outer)
  }

  def count(table: Table): Count = {
    val innerBody =
      s"""sql\"\"\"
          |  SELECT COUNT(*)
          |  FROM ${table.ref.fullName}
          |\"\"\".query[Long]
       """.stripMargin

    val inner = FunctionDef(Some(privateScope(table)), "countInner", Seq(), "Query0[Long]", innerBody)

    val outerBody = s"countInner().unique"
    val outer = FunctionDef(None, "count", Seq(), "ConnectionIO[Long]", outerBody)

    Count(inner, outer)
  }

  /* This should be split into pk and non-pk multigets - @todo */
  def multigets(table: Table): Seq[MultiGet] = {
    val rowType = rowNewType(table)

    val pkMultiget = pkNewType(table) match {

      case Some(pk) =>
        /* This is a bit of a hack and will need to change eventually */
        val pkMultigetInnerBodyCondition = table.primaryKeyColumns.map { c =>
          c.reference match {
            case Some(r) => s"${c.sqlName} = ANY($${{${c.scalaName}}.map(_.value).toArray})"
            case None => s"${c.sqlName} = ANY($${{${c.scalaName}}.toArray})"
          }
        }.mkString(" AND ")

        val pkMultigetInnerBody =
          s"""sql\"\"\"
              |  SELECT ${rowType._1.sqlColumns}
              |  FROM ${table.ref.fullName}
              |  WHERE $pkMultigetInnerBodyCondition
              |\"\"\".query[${rowType._2.symbol}]
       """.stripMargin

        val pkParams = pluralise(table.primaryKeyColumns.map(p => FunctionParam(p.scalaName, p.scalaType)))

        val pkMultigetInner = FunctionDef(Some(privateScope(table)), "multigetInner", pkParams, s"Query0[${rowType._2.symbol}]", pkMultigetInnerBody)

        val pkMultigetOuterBody =
          s"""multigetInner(${pkParams.map(_.name).head}).list"""

        val pkMultigetOuter = FunctionDef(None, "multiget", pkParams, s"ConnectionIO[List[${rowType._2.symbol}]]", pkMultigetOuterBody)

        Seq(MultiGet(pkMultigetInner, pkMultigetOuter))

      case None => Seq()
    }

    val fkMultigets = table.columns.flatMap {
      case c @ Column(colName, colType, copProps) if c.reference.isDefined && !c.isNullible =>

        val params = pluralise(List(FunctionParam(c.scalaName, c.scalaType)))

        val condition = c.reference match {
          case Some(ref) => s"${c.sqlName} = ANY($${{${c.scalaName}}.map(_.value).toArray})"
          case None => s"${c.sqlName} = ANY($${{${c.scalaName}}.toArray})"
        }

        val innerBody =
          s"""sql\"\"\"
              |  SELECT ${rowType._1.sqlColumns}
              |  FROM ${table.ref.fullName}
              |  WHERE $condition
              |\"\"\".query[${rowType._2.symbol}]
       """.stripMargin

        val inner = FunctionDef(Some(privateScope(table)), s"multigetBy${c.scalaName.capitalize}Inner", params, s"Query0[${rowType._2.symbol}]", innerBody)

        val outerBody = s"""${inner.name}(${params.head.name}).list"""

        val outer = FunctionDef(None, s"multigetBy${c.scalaName.capitalize}", params, s"ConnectionIO[List[${rowType._2.symbol}]]", outerBody)

        Seq(MultiGet(inner, outer))

      case _ => Seq()
    }

    pkMultiget ++ fkMultigets
  }

  /**
    * Turn a singular param into a multiple param.
    *
    * eg, (id: Long, name: String) => (params: Seq(Long, String))
    * eg. (id: Long) => (id: Seq(Long))
    */
  def pluralise(params: List[FunctionParam]): List[FunctionParam] = params match {
    case p :: Nil => List(p.copy(`type` = app(p.`type`, "Seq")))
    case ps => ps
  }

  /**
    * Wraps the given type in some applicative thing.
    */
  def app(t: ScalaType, app: String): ScalaType = ScalaType(s"$app[${t.symbol}]", s"$app(${t.arb})")


  implicit class ColumnScalaRep(column: sql.Column) {

    def scalaName: String = makeSafe(column.sqlName.camelCase)

    /* This drops any foreign keys after the first, because I'm not sure if that's even valid */
    def reference = column.properties.flatten {
      case r @ sql.References(_, _) => Some(r)
      case _ => None
    }.headOption


    def scalaType: ScalaType = {
      val base = reference match {
        case Some(sql.References(table, col)) =>
          val targetTable = resolve(table)
          val targetColumn = targetTable.columns.filter(_.sqlName.toLowerCase() == col.toLowerCase).head
          foreignType(targetTable, targetColumn)

        case None =>
          column.sqlType match {
            case sql.BigInt          => ScalaType("Long", "0L")
            case sql.BigSerial       => ScalaType("Long", "0L")
            case sql.Boolean         => ScalaType("Boolean", "true")
            case sql.DoublePrecision => ScalaType("Double", "0.0")
            case sql.Integer         => ScalaType("Int", "0")
            case sql.Text            => ScalaType("String", "\"\"")
            case sql.Timestamp       => ScalaType("Timestamp", "new Timestamp(0L)")
          }
      }

      column.isNullible match {
        case true => ScalaType(s"Option[${base.symbol}]", "None")
        case false => base
      }
    }

    def scalaRep = RowRepField(List(column), scalaName, scalaType)
  }

  /* This is the type that should be used by other tables referring to the given column */
  def foreignType(table: Table, column: Column): ScalaType = {

    val rowType = rowNewType(table)
    val orig = rowType._1.filter(_.source.headOption.contains(column)).head.scalaType

    val symbol = s"${targetObject(table)}.${orig.symbol}"
    val base = ScalaType(symbol, s"$symbol(${column.scalaType.arb})")

    column.isNullible match {
      case true => ScalaType(s"Option[${base.symbol}]", "None")
      case false => base
    }
  }
}
