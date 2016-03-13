package mdmoss.doobiegen

import mdmoss.doobiegen.sql.{CreateSchema, Statement, Table, CreateTable}

case class DbModel(tables: Seq[sql.Table])

object DbModel {

  def update(model: DbModel, sql: Statement): DbModel = sql match {

    case CreateTable(table, props) => model.copy(tables = model.tables :+ Table(table, props))

    /* Statements here have no effect on the model, at present */
    case CreateSchema(_) => model
  }

  def empty = DbModel(Seq())
}
