package mdmoss.doobiegen

import mdmoss.doobiegen.Runner.Target
import mdmoss.doobiegen.sql.TableRef

case class CodePlan(objects: Seq[ObjectPlan])

case class ObjectPlan(`package`: String, name: String, code: Seq[PlanPart])

sealed trait PlanPart

case class Insert(table: TableRef, fields: Seq[InsertParam]) extends PlanPart
case class InsertParam(column: sql.Column, scalaType: ScalaType)

case class ScalaType(symbol: String, arb: String)

object CodePlan {

  def gen(model: DbModel, target: Target): CodePlan = {
    /* Basic things first. Generate an object for each table. */
    val basics = model.tables.map { t =>
      ObjectPlan(target.`package` + ".gen", objectName(t.ref), Seq(genInsert(t)))
    }

    CodePlan(basics)
  }

  def genInsert(table: sql.Table): Insert = {
    val params = table.properties.flatMap {
      case c @ sql.Column(_, _, _) => Some(InsertParam(c, scalaType(c)))
      case _                       => None
    }
    Insert(table.ref, params)
  }

  def scalaType(column: sql.Column): ScalaType = {
    val base = column.sqlType match {
      case sql.BigInt          => ScalaType("Long", "0L")
      case sql.BigSerial       => ScalaType("Long", "0L")
      case sql.Boolean         => ScalaType("Boolean", "true")
      case sql.DoublePrecision => ScalaType("Double", "0.0")
      case sql.Integer         => ScalaType("Int", "0")
      case sql.Text            => ScalaType("String", "\"\"")
      case sql.Timestamp       => ScalaType("Timestamp", "new Timestamp(0L)")
    }

    column.nullible match {
      case true => ScalaType(s"Option[${base.symbol}]", "None")
      case false => base
    }
  }

  def objectName(tableRef: TableRef) = {
    """_([a-z])""".r.replaceAllIn(tableRef.sqlName, m => m.group(1).capitalize).capitalize
  }

}