package mdmoss.doobiegen

import org.parboiled2._

class SqlStatementParser(val input: ParserInput) extends Parser {

  def StatementLine = rule { Statement ~ EOI }

  def Statement = rule { CreateTable | CreateSchema }

  def CreateTable: Rule1[sql.Statement] = rule {
    ("CREATE TABLE " ~ TableRef ~ '(' ~ OptionalWhitespace ~ zeroOrMore(Column) ~ ");") ~>
       { (ref, columns) => sql.CreateTable(ref, columns) }
  }

  def Column: Rule1[sql.Column] = rule {
    (ValidIdentifier ~ Type ~ optional(',') ~ OptionalWhitespace) ~> {(id, typ) => sql.Column(id, typ) }
  }

  /* Note: all of these must be lowercase, and this isn't enforced by parboiled :/ */
  def Type: Rule1[sql.Type] = rule (
         ignoreCase("bigint")                   ~ push(sql.BigInt)
      |  ignoreCase("boolean")                  ~ push(sql.Boolean)
      |  ignoreCase("double precision")         ~ push(sql.DoublePrecision)
      |  ignoreCase("integer")                  ~ push(sql.Integer)
      |  ignoreCase("text")                     ~ push(sql.Text)
      |  ignoreCase("timestamp with time zone") ~ push(sql.Timestamp)
      |  ignoreCase("timestamp")                ~ push(sql.Timestamp)
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

