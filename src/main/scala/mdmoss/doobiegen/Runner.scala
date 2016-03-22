package mdmoss.doobiegen

import java.io.{File, PrintWriter}

import org.parboiled2.ParseError

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success}

object Runner {

  val config = Target(
    TestDatabase(
      "org.postgresql.Driver",
      "jdbc:postgresql:gen",
      "test",
      "test"
    ),
    srcDir    = "out/src/main/scala/mdmoss/doobiegen/db",
    testDir   = "out/src/test/scala/mdmoss/doobiegen/db",
    `package` = "mdmoss.doobiegen.db"
  )

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

    val plan = CodePlan.gen(model, config)
    plan.objects.foreach(println)

    println(seperator)

    val code = Code.gen(plan, config)

    /* Delete everything currently in the target directories */
    val dirs = List(new File(config.srcDir), new File(config.testDir))
    dirs.foreach(_.mkdirs())
    dirs.foreach(_.listFiles().forall(_.delete()))

    code.src.foreach { f =>
      new PrintWriter(s"${config.srcDir}/${f.name}") {write(f.contents); close()}
    }
    code.tests.foreach { f =>
      new PrintWriter(s"${config.testDir}/${f.name}") {write(f.contents); close()}
    }


  }

  val seperator = "*" * 80

  case class TestDatabase(driver: String, url: String, username: String, password: String)
  case class Target(testDb: TestDatabase, srcDir: String, testDir: String, `package`: String)
}
