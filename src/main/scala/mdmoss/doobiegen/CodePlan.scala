package mdmoss.doobiegen

import mdmoss.doobiegen.sql.TableRef

case class CodePlan(objects: Seq[ObjectPlan])

case class ObjectPlan(`package`: String, name: String, code: Seq[PlanPart])

sealed trait PlanPart

case class Insert(table: TableRef, fields: Seq[InsertParam]) extends PlanPart
case class InsertParam(column: sql.Column, scalaType: ScalaType)

case class ScalaType(symbol: String)

object CodePlan {

  def gen(model: DbModel): CodePlan = {
    /* Basic things first. Generate an object for each table. */
    val basics = model.tables.map { t =>
      ObjectPlan("", t.ref.sqlName, Seq(genInsert(t)))
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
      case sql.BigInt          => ScalaType("Long")
      case sql.BigSerial       => ScalaType("Long")
      case sql.Boolean         => ScalaType("Boolean")
      case sql.DoublePrecision => ScalaType("Double")
      case sql.Integer         => ScalaType("Integer")
      case sql.Text            => ScalaType("String")
      case sql.Timestamp       => ScalaType("Timestamp")
    }

    base
  }

}