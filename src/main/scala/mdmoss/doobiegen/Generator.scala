package mdmoss.doobiegen

import mdmoss.doobiegen.output.File
import mdmoss.doobiegen.sql.Table

class Generator(analysis: Analysis) {

  val a = analysis
  val db = analysis.model
  val target = analysis.target

  def gen: Seq[File] = {
    /* First aim - objects for each database table */

    val tableFiles = db.tables.map { t =>

      val contents =
        s"""package ${a.targetPackage(t)}
            |
            |/* Todo handle imports better */
            |import doobie.imports._
            |import java.sql.Timestamp
            |
            |object ${a.targetObject(t)} {
            |
            |${genPkNewType(t)}
            |
            |
            |}
         """.stripMargin


      File(
        a.targetPackage(t),
        a.targetObject(t) + ".scala",
        contents,
        isTest = false
      )
    }

    tableFiles
  }

  def genPkNewType(table: Table): String = {
    a.pkNewType(table).map { pk =>
      s"case class ${pk._2.symbol}(${pk._1.map(f => s"${f.scalaName}: ${f.scalaType.symbol}").mkString(", ")})"
    }.getOrElse("")



  }

}
