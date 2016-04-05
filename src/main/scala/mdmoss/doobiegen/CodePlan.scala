package mdmoss.doobiegen

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

case class Create(insert: Insert, rowRep: RowRep) extends PlanPart

case class ScalaType(symbol: String, arb: String)
