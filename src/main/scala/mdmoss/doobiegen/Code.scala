package mdmoss.doobiegen

import mdmoss.doobiegen.Runner.Target

object Code {

  case class CodegenResult(src: Seq[OutFile], tests: Seq[OutFile])

  def gen(plan: CodePlan, target: Target): CodegenResult = {
    val src = plan.objects.map { o =>
      val name = s"${o.name}.scala"

      val parts = o.code.flatMap {
        case i @ Insert(_, _) => Some(genInsert(i))
        case _ => None
      }

      val contents =
        s"""package ${o.`package`}
           |
           |/* Todo handle imports */
           |import doobie.imports._
           |import java.sql.Timestamp
           |
           |object ${o.name} {
           |
           |${parts.map(_.pp).mkString("\n").indent(2)}
           |
           |}
         """.stripMargin


      (o, parts, OutFile(name, contents))
    }

    val tests = src.map { case (obj, parts, code) =>
      val name = s"${obj.name}Spec.scala"

      val testParts = parts.flatMap {
        case f @ FunctionDef(_, _, "Update0", _) => Some(checkTest(f))
        case _ => None
      }

      val contents =
        s"""package ${obj.`package`}
           |
           |/* Todo handle imports */
           |import doobie.imports._
           |import java.sql.Timestamp
           |import org.specs2.mutable.Specification
           |import scalaz.concurrent.Task
           |import doobie.contrib.specs2.analysisspec.AnalysisSpec
           |
           |object ${obj.name}Spec extends Specification with AnalysisSpec {
           |
           |  val transactor = DriverManagerTransactor[Task](
           |    "${target.testDb.driver}",
           |    "${target.testDb.url}",
           |    "${target.testDb.username}",
           |    "${target.testDb.password}"
           |  )
           |
           |${testParts.map(_.pp).mkString("\n").indent(2)}
           |
           |}
         """.stripMargin

      OutFile(name, contents)
    }

    CodegenResult(src.map(_._3), tests)
  }

  def genInsert(insert: Insert): CodePart = {

    val params = insert.fields.map { f =>
      FunctionParam(f.column.sqlName, f.scalaType)
    }

    val body =
      s"""sql\"\"\"
        |  INSERT INTO ${insert.table.sqlName} (${insert.fields.map(_.column.sqlName).mkString(", ")})
        |  VALUES (${params.map(_.name).mkString(", ")})
        |\"\"\".update
      """.stripMargin.trim

    FunctionDef("insert", params, "Update0", body)
  }

  def checkTest(f: FunctionDef): Block = Block(s"check(${f.name})")

  implicit class indentString(s: String) {
    def indent(nSpaces: Int) = s.split("\n").map(p => " " * nSpaces + p).mkString("\n")
  }
}

import Code._

case class OutFile(name: String, contents: String)

sealed trait CodePart {def pp: String}

case class Import(`package`: String) extends CodePart {
  def pp = s"import ${`package`}"
}

case class FunctionDef(name: String, params: Seq[FunctionParam], returnType: String, body: String) extends CodePart {
  def pp =
    s"""def $name(${params.map{p => s"${p.name}: ${p.`type`.symbol}"}.mkString(", ")}): $returnType = {
       |${body.indent(2)}
       |}
     """.stripMargin
}
case class FunctionParam(name: String, `type`: ScalaType)

case class Block(body: String) extends CodePart {
  override def pp: String = body
}