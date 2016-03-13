package mdmoss.doobiegen

object sql {

  sealed trait Type
  case object BigInt          extends Type
  case object BigSerial       extends Type
  case object Boolean         extends Type
  case object DoublePrecision extends Type
  case object Integer         extends Type
  case object Text            extends Type
  case object Timestamp       extends Type

  sealed trait TableProperty
  case class Column(sqlName: String, sqlType: Type) extends TableProperty

  case class TableRef(schema: Option[String], sqlName: String)

  sealed trait Statement
  case class CreateTable(table: TableRef, properties: Seq[TableProperty]) extends Statement

  case class CreateSchema(name: String) extends Statement
}
