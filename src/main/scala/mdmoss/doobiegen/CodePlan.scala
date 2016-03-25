package mdmoss.doobiegen

import mdmoss.doobiegen.Runner.Target
import mdmoss.doobiegen.sql.TableRef

case class CodePlan(objects: Seq[ObjectPlan])

case class ObjectPlan(`package`: String, name: String, code: Seq[PlanPart])

sealed trait PlanPart

case class Insert(table: TableRef, fields: Seq[InsertParam]) extends PlanPart
case class InsertParam(column: sql.Column, scalaType: ScalaType)

case class PKNewtype(columns: Seq[sql.Column], scalaType: ScalaType) extends PlanPart {
  def isComposite = columns.length > 1
}

case class RowRep(table: TableRef, fields: Seq[RowRepField], scalaType: ScalaType) extends PlanPart
case class RowRepField(name: String, `type`: ScalaType)

case class Create(insert: Insert, rowRep: RowRep) extends PlanPart

case class ScalaType(symbol: String, arb: String)

object CodePlan {

  def gen(model: DbModel, target: Target): CodePlan = {
    /* Basic things first. Generate an object for each table. */
    val basics = model.tables.map { t =>
      val parts = {
        val insert = genInsert(t)
        val pk = genPk(t)
        val rowRep = genRowRep(t, pk)
        val create = genCreate(t, insert, rowRep)

        Seq(insert) ++ pk.toSeq ++ Seq(rowRep, create)
      }

      ObjectPlan(target.`package` + ".gen", t.ref.scalaName, parts)
    }

    CodePlan(basics)
  }

  def genInsert(table: sql.Table): Insert = {
    val params = table.properties.flatMap {
      case c @ sql.Column(_, _, _) => Some(InsertParam(c, getScalaType(c)))
      case _                       => None
    }
    Insert(table.ref, params)
  }

  def genPk(table: sql.Table): Option[PKNewtype] = {

      table.primaryKeyColumns.length match {
      case 0 => None
      case _ =>
        val scalaTypeName = table.ref.scalaName + (table.primaryKeyColumns match {
          case c :: Nil => c.scalaName.capitalize
          case _ => "PrimaryKey"
        })

        val arb = s"$scalaTypeName(" + table.primaryKeyColumns.map(getScalaType).map(_.arb).mkString(", ") + ")"

        val scalaType = ScalaType(scalaTypeName, arb)
        Some(PKNewtype(table.primaryKeyColumns, scalaType))
    }
  }

  def genRowRep(t: sql.Table, pk: Option[PKNewtype]): RowRep = {

    val primaryKey = pk.map { p =>
      p.columns.toList match {
        case c :: Nil => RowRepField(c.scalaName, p.scalaType)
        case _        => RowRepField("primaryKey", p.scalaType)
      }
    }.toSeq

    val nonPrimaryKeys = t.columns.filterNot(t.primaryKeyColumns.contains).map { c =>
      RowRepField(c.scalaName, getScalaType(c))
    }

    val fields = primaryKey ++ nonPrimaryKeys

    val scalaTypeName = t.ref.scalaName.capitalize + "Row"
    val arb = s"$scalaTypeName(" + fields.map(_.`type`).map(_.arb).mkString(", ") + ")"

    RowRep(t.ref, fields, ScalaType(scalaTypeName, arb))
  }

  def genCreate(t: sql.Table, insert: Insert, rowRep: RowRep): Create = {
    Create(insert, rowRep)
  }

  def getScalaType(column: sql.Column): ScalaType = {
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

  implicit class TableRefProps(t: sql.TableRef) {
    def scalaName = t.sqlName.camelCase.capitalize
  }

  implicit class ColumnProps(c: sql.Column) {
    def scalaName = c.sqlName.camelCase
  }

  implicit class camelCaseStrings(s: String) {
    def camelCase: String = """_([a-z])""".r.replaceAllIn(s, m => m.group(1).capitalize)
  }
}