package mdmoss.doobiegen

import mdmoss.doobiegen.Runner.Target

object Code {

  case class CodegenResult(src: Seq[OutFile], tests: Seq[OutFile])

  import CodePlan._

  def gen(plan: CodePlan, target: Target): CodegenResult = {
    val src = plan.objects.map { o =>
      val name = s"${o.name}.scala"

      val parts = o.code.flatMap {
        case i @ Insert(_, _) => Some(genInsert(i, target))
        case p @ PKNewtype(_, _) => Some(genPkNewtype(p, target))
        case r @ RowRep(_, _, _) => Some(genRowRep(r, target))
        case c @ Create(_, _) => Some(genCreate(c, target))
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
           |}
         """.stripMargin

      (o, parts, OutFile("gen", name, contents))
    }

    val tests = src.map { case (obj, parts, code) =>
      val name = s"${obj.name}Spec.scala"

      val testParts = parts.flatMap {
        case f @ FunctionDef(_, _, _, "Update0", _) => Some(checkTest(obj, f))
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

      OutFile("gen", name, contents)
    }

    CodegenResult(src.map(_._3), tests)
  }

  def genInsert(insert: Insert, target: Target): CodePart = {

    val params = insert.fields.map { f =>
      FunctionParam(f.column.scalaName, f.scalaType)
    }

    val tableRef = insert.table

    val body =
      s"""sql\"\"\"
        |  INSERT INTO ${insert.table.fullName} (${insert.fields.map(_.column.sqlName).mkString(", ")})
        |  VALUES (${params.map(_.name).map(s => s"$$$s").mkString(", ")})
        |\"\"\".update
      """.stripMargin.trim

    FunctionDef(target.enclosingPackage, "insert", params, "Update0", body)
  }

  def genPkNewtype(pk: PKNewtype, target: Target): CodePart = {

    val types = pk.columns.zip(pk.columns.map(CodePlan.getScalaType))

    CaseClassDef(pk.scalaType.symbol, types.map(t => CaseClassField(t._1.sqlName, t._2)))
  }

  def genRowRep(row: RowRep, target: Target): CodePart = {
    CaseClassDef(row.table.scalaName + "Row", row.fields.map(f => CaseClassField(f.name, f.`type`)))
  }

  def genCreate(create: Create, target: Target): CodePart = {

    val params = create.insert.fields.map { f =>
      FunctionParam(f.column.scalaName, f.scalaType)
    }
/*
    val body =
      s"""
         |insert(${params.map(_.name).mkString(", ")}).withUniqueGeneratedKeys[${create.rowRep.scalaType.symbol}]
         |  (${create.rowRep.fields.map(_.)})
         |
       """.stripMargin*/

    FunctionDef(None, "create", params, create.rowRep.scalaType.symbol, "")
  }

  def checkTest(o: ObjectPlan, f: FunctionDef): Block = Block(
    s"check(${o.name}.${f.name}(${f.params.map(_.`type`.arb).mkString(", ")}))"
  )

  implicit class indentString(s: String) {
    def indent(nSpaces: Int) = s.split("\n").map(p => " " * nSpaces + p).mkString("\n")
  }
}

import Code._

case class OutFile(path: String, name: String, contents: String)

sealed trait CodePart {def pp: String}

case class Import(`package`: String) extends CodePart {
  def pp = s"import ${`package`}"
}

case class FunctionDef(privatePkg: Option[String], name: String, params: Seq[FunctionParam], returnType: String, body: String) extends CodePart {
  def pp = {
    val prv = privatePkg.map(p => s"private[$p] ").getOrElse("")

    s"""${prv}def $name(${params.map { p => s"${p.name}: ${p.`type`.symbol}" }.mkString(", ")}): $returnType = {
       |${body.indent(2)}
       |}
     """.stripMargin
  }
}
case class FunctionParam(name: String, `type`: ScalaType)

case class CaseClassDef(name: String, fields: Seq[CaseClassField]) extends CodePart {
  def pp = {
    s"case class $name(${fields.map(f => s"${f.name}: ${f.`type`.symbol}").mkString(", ")})"
  }
}
case class CaseClassField(name: String, `type`: ScalaType)

case class Block(body: String) extends CodePart {
  override def pp: String = body
}