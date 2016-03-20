package mdmoss.doobiegen

object Code {
  def gen(plan: CodePlan): Seq[OutFile] = {
    plan.objects.map { o =>
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
           |${parts.map(_.pp).mkString("\n")}
           |
           |}
         """.stripMargin


      OutFile(name, contents)
    }
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

    FunctionDef(insert.table.sqlName, params, "Update0", body)
  }

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