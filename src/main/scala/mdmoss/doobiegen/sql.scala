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
    def references: Option[sql.References] = properties.flatten {
      case r @ References(_, _) => Some(r)
      case _ => None
    }.headOption
  }

  case class CompositePrimaryKey(columnNames: Seq[String]) extends TableProperty

  case class CompositeUnique(columnNames: Seq[String]) extends TableProperty

  case class TableRef(schema: Option[String], sqlName: String) {
    /* I don't like this name. @todo change this. */
    /* Also, see case notes on Analysis.RowRepsForInsert.sqlColumns. */
    def fullName = schema.map(s => s"${s.toLowerCase}.").getOrElse("") + sqlName.toLowerCase
  }

  sealed trait Statement
  case class CreateTable(table: TableRef, properties: Seq[TableProperty]) extends Statement
  case class CreateSchema(name: String) extends Statement
  case class AlterTable(table: TableRef, action: AlterTableAction) extends Statement
  case class DropTable(table: TableRef) extends Statement





  sealed trait AlterTableAction
  case class AddProperty(tableProperty: TableProperty) extends AlterTableAction
  case class DropColumn(column: String) extends AlterTableAction
  case class DropColumnProperty(column: String, property: sql.ColumnProperty) extends AlterTableAction

  case object Ignored extends Statement

  case class Table(ref: TableRef, properties: Seq[TableProperty]) {
    def columns = properties.flatten {
      case c @ Column(_, _, _) => Some(c)
      case _ => None
    }.toList

    /* We're overloading this for now, because we only do special things with a single pk column */
    def primaryKeyColumns = columns.filter(_.properties.contains(PrimaryKey))
/*      ++ properties.flatMap {
        case CompositePrimaryKey(names) => columns.filter(c => names.contains(c.sqlName))
        case _ => Seq()
      }.toList*/

    def nonPrimaryKeyColumns = columns.filterNot(primaryKeyColumns.contains)
  }

  sealed trait ColumnProperty
  case object Null                                       extends ColumnProperty
  case object NotNull                                    extends ColumnProperty
  case object PrimaryKey                                 extends ColumnProperty
  case object Default                                    extends ColumnProperty
  case class References(table: TableRef, column: String) extends ColumnProperty
  case object Unique                                     extends ColumnProperty
  case object Constraint                                 extends ColumnProperty
}
