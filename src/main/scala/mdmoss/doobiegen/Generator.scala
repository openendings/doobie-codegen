package mdmoss.doobiegen

import mdmoss.doobiegen.output.File

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
            |/* Todo handle imports */
            |import doobie.imports._
            |import java.sql.Timestamp
            |
            |object ${a.targetObject(t)} {
            |
            |  ${a.pkNewType(t)}
            |
            |
            |}
         """.stripMargin


      File(
        a.targetPackage(t),
        a.targetObject(t) + ".scala",
        "",
        isTest = false
      )
    }






    tableFiles
  }

}
