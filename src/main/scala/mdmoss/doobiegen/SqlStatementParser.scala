package mdmoss.doobiegen

import org.parboiled2._

class SqlStatementParser(val input: ParserInput) extends Parser {

  def StatementLine = rule { Statement ~ EOI }

  def Statement = rule { CreateTable | CreateSchema }

  def CreateTable: Rule1[sql.Statement] = rule {
    ("CREATE TABLE " ~ optional("IF NOT EXISTS ") ~ TableRef ~ '(' ~ OptionalWhitespace ~ zeroOrMore(TableProperty) ~ ");") ~>
       { (ref, columns) => sql.CreateTable(ref, columns) }
  }

  def TableProperty: Rule1[sql.TableProperty] = rule { Column | CompositePrimaryKey }

  def Column: Rule1[sql.Column] = rule {
    (ValidIdentifier ~ Type ~ zeroOrMore(ColumnProperty ~ optional(' ')) ~ optional(',') ~ OptionalWhitespace) ~>
      { (id, typ, props) => sql.Column(id, typ, props) }
  }

  def CompositePrimaryKey: Rule1[sql.CompositePrimaryKey] = rule {
    ("PRIMARY KEY" ~ '(' ~ oneOrMore(ValidIdentifier ~ optional(',') ~ OptionalWhitespace) ~ ')' ~ OptionalWhitespace) ~>
      { pkn: Seq[String] => sql.CompositePrimaryKey(pkn) }
  }

  /* Note: all of these must be lowercase, and this isn't enforced by parboiled :/ */
  def Type: Rule1[sql.Type] = rule {(
      ignoreCase("bigint") ~                       push(sql.BigInt)
        | ignoreCase("boolean") ~                  push(sql.Boolean)
        | ignoreCase("double precision") ~         push(sql.DoublePrecision)
        | ignoreCase("integer") ~                  push(sql.Integer)
        | ignoreCase("text") ~                     push(sql.Text)
        | ignoreCase("timestamp with time zone") ~ push(sql.Timestamp)
        | ignoreCase("timestamp") ~                push(sql.Timestamp)
      ) ~ OptionalWhitespace
  }

  def ColumnProperty: Rule1[sql.ColumnProperty] = rule (
      ignoreCase("null")     ~ push(sql.Null)
    | ignoreCase("not null") ~ push(sql.NotNull)
    | ignoreCase("primary key") ~ push(sql.PrimaryKey)
    | ignoreCase("default") ~ " " ~ oneOrMore(CharPredicate.Alpha ++ '_') ~ push(sql.Default)
  )

  /* http://www.postgresql.org/docs/9.4/static/sql-syntax-lexical.html */
  def TableRef: Rule1[sql.TableRef] = rule {
    optional(ValidIdentifier ~ '.') ~ ValidIdentifier ~> { (schema, name) => sql.TableRef(schema, name) }
  }

  def ValidIdentifier: Rule1[String] = rule {
    capture (
      (CharPredicate.Alpha ++ '_') ~ zeroOrMore(CharPredicate.Alpha ++ CharPredicate.Digit ++ '_' ++ '$')
    ) ~ OptionalWhitespace
  }

  def Whitespace = rule { oneOrMore(anyOf(" \n\t")) }
  def OptionalWhitespace = rule { zeroOrMore(anyOf(" \n\t")) }

  def CreateSchema: Rule1[sql.Statement] = rule {
    ("CREATE SCHEMA " ~ ValidIdentifier ~ ';') ~> { (name) => sql.CreateSchema(name) }
  }
}

