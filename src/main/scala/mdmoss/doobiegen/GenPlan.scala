package mdmoss.doobiegen

import mdmoss.doobiegen.Runner.Target
import mdmoss.doobiegen.sql.{Column, Table}

trait CodeBits {def parts: Seq[CodePart]}

class GenPlan(model: DbModel, table: Table, target: Target) {

  def targetPackage = target.`package` + table.ref.schema.map(s => s".$s").getOrElse("") + ".gen"

  def targetObject = table.ref.sqlName.camelCase.capitalize

  def privateScope = targetPackage.split('.').reverse.head

  case class RowRepField(source: List[Column], scalaName: String, scalaType: ScalaType)

  def pkNewType: Option[(List[RowRepField], ScalaType)] = table.primaryKeyColumns match {
    case Nil      => None
    case c :: Nil =>
      val rep = c.scalaRep.copy(scalaName = "value")
      Some(List(rep), ScalaType(targetObject + c.sqlName.camelCase.capitalize, c.scalaType.arb))
    case cs       =>
      val name = targetObject + "PrimaryKey"
      val arb = merge(name, cs.map(_.scalaType))
      Some(cs.map(_.scalaRep), ScalaType(name, arb))
  }

  def rowNewType: (List[RowRepField], ScalaType) = {
    /* We'll put the primary key first, if any, then other components */
    val pkPart = pkNewType.map { case (reps, newType) => reps match {
        case r :: Nil => RowRepField(r.source, r.scalaName, newType)
        case rs       => RowRepField(rs.flatMap(_.source), "pk", newType)
      }
    }

    val parts = pkPart.toList ++ table.nonPrimaryKeyColumns.map(_.scalaRep)
    (parts, ScalaType(targetObject + "Row", merge(targetObject + "Row", parts.map(_.scalaType))))
  }

  case class Insert(fn: FunctionDef) extends CodeBits { def parts: Seq[CodePart] = Seq(fn) }

  def insert: Insert = {
    val params = rowNewType._1.map(t => FunctionParam(t.scalaName, t.scalaType))
    val body =
      s"""sql\"\"\"
          |  INSERT INTO ${table.ref.fullName} (${rowNewType._1.sqlColumns})
          |  VALUES (${params.map(_.name).map(s => s"$$$s").mkString(", ")})
          |\"\"\".update
      """.stripMargin.trim

    val fn = FunctionDef(Some(privateScope), "insert", params, "Update0", body)
    Insert(fn)
  }





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
}
