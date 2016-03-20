package mdmoss.doobiegen

import org.parboiled2.ParseError

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success}

object Runner {

  def main(args: Array[String]) {

     /* We could do something more intelligent here, but this works for now */
    val sql = {
      import scala.sys.process._
      val buffer = new ListBuffer[String]()
      val proc = ("bash" :: "-c" :: "cat sql/*.sql" :: Nil).run(ProcessLogger(s => buffer.append(s)))
      proc.exitValue()
      buffer.mkString("\n")
    }

    /* We need to clean the input a little */
    import ParserUtils.SqlStringHelpers
    val cleanedAndSplit = sql
      .stripComments
      .splitOnUnescapedSemicolons
      .map(_.trim)

    val parsers = cleanedAndSplit.map(new SqlStatementParser(_))

    parsers.foreach { s =>
      println(seperator)
      println(s.input.sliceString(0, s.input.length))
      s.StatementLine.run() match {
        case r @ Success(_) => println(r)
        case r @ Failure(f) => f match {
          case e @ ParseError(_, _, _) => println(s.formatError(e))
        }
      }
    }

    val statements = parsers.flatMap(_.StatementLine.run().toOption)

    println(seperator)

    if (statements.length != parsers.length) throw new Throwable("Failed parsing. Exiting.")

    val model = statements.foldLeft(DbModel.empty)(DbModel.update)
    model.tables.foreach(println)

    println(seperator)

    val plan = CodePlan.gen(model)
    plan.part.foreach(println)

  }

  val seperator = "*" * 80
}
