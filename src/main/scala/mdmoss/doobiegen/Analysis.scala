package mdmoss.doobiegen

import mdmoss.doobiegen.Analysis.CodeBits
import mdmoss.doobiegen.Runner.Target
import mdmoss.doobiegen.sql.{Column, Table}

case class RowRepField(source: List[Column], scalaName: String, scalaType: ScalaType)

case class Insert(fn: FunctionDef)

case class Create(fn: FunctionDef)

case class Get(inner: FunctionDef, outer: FunctionDef)

object Analysis {

  /* Helpers */
  implicit class CamelCaseStrings(s: String) {
    def camelCase: String = """_([a-z])""".r.replaceAllIn(s, m => m.group(1).capitalize)
  }

  implicit class ColumnScalaRep(column: sql.Column) {

    def scalaName: String = column.sqlName.camelCase

    def scalaType: ScalaType = {
      val base = column.sqlType match {
        case sql.BigInt          => ScalaType("Long", "0L")
        case sql.BigSerial       => ScalaType("Long", "0L")
        case sql.Boolean         => ScalaType("Boolean", "true")
        case sql.DoublePrecision => ScalaType("Double", "0.0")
        case sql.Integer         => ScalaType("Int", "0")
        case sql.Text            => ScalaType("String", "\"\"")
        case sql.Timestamp       => ScalaType("Timestamp", "new Timestamp(0L)")
      }

      column.isNullible match {
        case true => ScalaType(s"Option[${base.symbol}]", "None")
        case false => base
      }
    }

    def scalaRep = RowRepField(List(column), scalaName, scalaType)
  }

  implicit class RowRepsForInsert(l: List[RowRepField]) {
    def sqlColumns: String = l.flatMap(_.source).map(_.sqlName).mkString(", ")
  }


  /** Returns an arbitrary using the given constructor and the arb instance for each type in order */
  def merge(constructor: String, scalaTypes: List[ScalaType]): String = {
    s"$constructor(" + scalaTypes.map(_.arb).mkString(", ") + ")"
  }

  trait CodeBits { def parts: Seq[CodePart] }
}

class Analysis(val model: DbModel, val target: Target) {
  import Analysis._

  def targetPackage(table: Table) = target.`package` + table.ref.schema.map(s => s".$s").getOrElse("") + ".gen"

  def targetObject(table: Table) = table.ref.sqlName.camelCase.capitalize

  def privateScope(table: Table) = target.`package`.split('.').reverse.head

  def pkNewType(table: Table): Option[(List[RowRepField], ScalaType)] = table.primaryKeyColumns match {
    case Nil      => None
    case c :: Nil =>
      val rep = c.scalaRep.copy(scalaName = "value")
      val symbol = c.sqlName.camelCase.capitalize
      val fullyQualified = s"${targetPackage(table)}.${targetObject(table)}.$symbol"
      Some((List(rep), ScalaType(symbol, s"$fullyQualified(${c.scalaType.arb})")))
    case cs       =>
      val name = "PrimaryKey"
      val arb = merge(name, cs.map(_.scalaType))
      Some((cs.map(_.scalaRep), ScalaType(name, arb)))
  }

  def rowNewType(table: Table): (List[RowRepField], ScalaType) = {
    /* We'll put the primary key first, if any, then other components */
    val pkPart = pkNewType(table).map { case (reps, newType) => reps match {
      case r :: Nil => RowRepField(r.source, r.scalaName, newType)
      case rs       => RowRepField(rs.flatMap(_.source), "pk", newType)
    }
    }

    val parts = pkPart.toList ++ table.nonPrimaryKeyColumns.map(_.scalaRep)
    (parts, ScalaType("Row", merge(targetObject(table) + "Row", parts.map(_.scalaType))))
  }

  def insert(table: Table): Insert = {
    val params = rowNewType(table)._1.map(t => FunctionParam(t.scalaName, t.scalaType))
    val body =
      s"""sql\"\"\"
          |  INSERT INTO ${table.ref.fullName} (${rowNewType(table)._1.sqlColumns})
          |  VALUES (${params.map(_.name).map(s => s"$$$s").mkString(", ")})
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

    val fn = FunctionDef(Some(privateScope(table)), "create", in.fn.params, s"ConnectionIO[${rowType._2.symbol}]", body)
    Create(fn)
  }

  def get(table: Table): Option[Get] = pkNewType(table).map { pk =>

    val rowType = rowNewType(table)

    val innerBody =
      s"""sql\"\"\"
         |  SELECT ${rowType._1.flatMap(_.source).mkString(", ")}
         |  FROM ${table.ref.fullName}
         |  WHERE ${pk._1.flatMap(_.source).mkString(", ")} = id
         |\"\"\".query[${pk._2.symbol}]
       """.stripMargin

    val inner = FunctionDef(Some(privateScope(table)), "getInner", Seq(FunctionParam("id", pk._2)), rowType._2.symbol, innerBody)

    Get(inner, inner)
  }
}
