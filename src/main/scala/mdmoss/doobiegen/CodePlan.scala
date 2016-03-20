package mdmoss.doobiegen

import mdmoss.doobiegen.sql.TableRef

case class CodePlan(part: Seq[PlanPart])

sealed trait PlanPart

case class ScalaType(symbol: String)

case class InsertParam(column: sql.Column, scalaType: ScalaType)

case class Insert(table: TableRef, fields: Seq[InsertParam]) extends PlanPart

object CodePlan {

  def gen(model: DbModel): CodePlan = {

    val inserts = genInserts(model)

    CodePlan(inserts)
  }

  def genInserts(model: DbModel): Seq[Insert] = {
    model.tables.map { t =>
      val params = t.properties.flatMap {
        case c @ sql.Column(_, _, _) => Some(InsertParam(c, scalaType(c)))
        case _                       => None
      }

      Insert(t.ref, params)
    }
  }

  def scalaType(column: sql.Column): ScalaType = {
    val base = column.sqlType match {
      case sql.BigInt          => ScalaType("Long")
      case sql.Boolean         => ScalaType("Boolean")
      case sql.DoublePrecision => ScalaType("Double")
      case sql.Integer         => ScalaType("Integer")
      case sql.Text            => ScalaType("String")
      case sql.Timestamp       => ScalaType("Timestamp")
    }

    base
  }

}