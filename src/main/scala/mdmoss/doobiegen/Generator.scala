package mdmoss.doobiegen

import mdmoss.doobiegen.output.File

class Generator(analysis: Analysis) {

  val a = analysis
  val db = analysis.model
  val target = analysis.target

  def gen: Seq[File] = {
    /* First aim - objects for each database table */

    val tableFiles = db.tables.map { t =>

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
