package mdmoss.doobiegen

object Code {
  def gen(plan: CodePlan): Seq[OutFile] = {
    plan.objects.map { o =>
      OutFile(o.`package`, o.name, Seq(), Seq())
    }
  }

  def genInsert(insert: Insert): CodePart = {

    val params = insert.fields.map { f =>
      FunctionParam(f.column.sqlName, f.scalaType)
    }

    FunctionDef(insert.table.sqlName, params, "", "")
  }
}

case class OutFile(`package`: String, name: String, imports: Seq[String], parts: Seq[CodePart])

sealed trait CodePart

case class FunctionDef(name: String, params: Seq[FunctionParam], returnType: String, body: String) extends CodePart

case class FunctionParam(name: String, `type`: ScalaType)