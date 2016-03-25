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
  case class Column(sqlName: String, sqlType: Type, properties: Seq[ColumnProperty]) extends TableProperty {
    def isNullible = properties.contains(Null) || (!properties.contains(NotNull) && !properties.contains(PrimaryKey))
  }

  case class CompositePrimaryKey(columnNames: Seq[String]) extends TableProperty

  case class TableRef(schema: Option[String], sqlName: String) {
    /* I don't like this name. @todo change this. */
    def fullName = schema.map(s => s"$s.").getOrElse("") + sqlName
  }

  sealed trait Statement
  case class CreateTable(table: TableRef, properties: Seq[TableProperty]) extends Statement

  case class CreateSchema(name: String) extends Statement

  case class Table(ref: TableRef, properties: Seq[TableProperty]) {
    def columns = properties.flatten {
      case c @ Column(_, _, _) => Some(c)
      case _ => None
    }
    def primaryKeyColumns = columns.filter(_.properties.contains(PrimaryKey)) ++
      properties.flatMap {
        case CompositePrimaryKey(names) => columns.filter(c => names.contains(c.sqlName))
        case _ => Seq()
      }
  }

  sealed trait ColumnProperty
  case object Null       extends ColumnProperty
  case object NotNull    extends ColumnProperty
  case object PrimaryKey extends ColumnProperty
}
