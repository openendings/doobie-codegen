package mdmoss.doobiegen

import mdmoss.doobiegen.output.File
import mdmoss.doobiegen.sql.Table
import Analysis._

class Generator(analysis: Analysis) {

  val a = analysis
  val db = analysis.model
  val target = analysis.target
  val tr = target.testDb

  def gen: Seq[File] = {
    /* First aim - objects for each database table */

    val tableFiles = db.tables.map { t =>

      val contents =
        s"""package ${a.targetPackage(t)}
            |
            |/* Todo handle imports better */
            |import doobie.imports._
            |import java.sql.Timestamp
            |import doobie.contrib.postgresql.pgtypes._
            |import scalaz._, Scalaz._
            |${genImports(t)}
            |
            |object ${a.targetObject(t)} {
            |  ${genMisc(t)}
            |
            |  ${genPkNewType(t)}
            |
            |  ${genRowType(t)}
            |
            |  ${genShapeType(t)}
            |
            |  ${ppFunctionDef(a.insert(t).fn)}
            |
            |  ${ppFunctionDef(a.insertMany(t).fn)}
            |
            |  ${ppFunctionDef(a.create(t).fn)}
            |
            |  ${ppFunctionDef(a.createMany(t).process)}
            |
                ${ppFunctionDef(a.createMany(t).list)}

            |
            |  ${a.get(t).map { g =>
                  ppFunctionDef(g.inner) + "\n" +
                  ppFunctionDef(g.outer)
                }.getOrElse("")}
            |
            |  ${a.find(t).map { f =>
                  ppFunctionDef(f.inner) + "\n" +
                  ppFunctionDef(f.outer)
               }.getOrElse("")}
            |
            |
            |  ${ppFunctionDef(a.all(t).inner)}
            |  ${ppFunctionDef(a.all(t).outer)}
            |
            |  ${ppFunctionDef(a.allUnbounded(t).fn)}
            |
            |  ${ppFunctionDef(a.count(t).inner)}
            |  ${ppFunctionDef(a.count(t).outer)}
            |
            |  ${a.baseMultiget(t).map { f => ppFunctionDef(f.fn) }.getOrElse("")}
            |
            |  ${a.multigets(t).map { m => ppFunctionDef(m.inner) }.mkString("\n") }
            |
            |  ${a.update(t).map { u =>
                  ppFunctionDef(u.inner) + "\n" +
                  ppFunctionDef(u.outer)
              }.getOrElse("")}
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

    /* We're going to follow up with tests */
    val testFiles = db.tables.map { t =>
      val contents =
        s"""package ${a.targetPackage(t)}
            |
            |/* Todo handle imports better */
            |import doobie.imports._
            |import java.sql.Timestamp
            |import org.specs2.mutable.Specification
            |import scalaz.concurrent.Task
            |import doobie.contrib.specs2.analysisspec.AnalysisSpec
            |import scalaz._, Scalaz._
            |
            |object ${a.targetObject(t)}Spec extends Specification with AnalysisSpec {
            |
            |  val transactor = DriverManagerTransactor[Task]("${tr.driver}", "${tr.url}", "${tr.username}", "${tr.password}")
            |
            |  ${checkTest(t, a.insert(t).fn)}
            |
            |  ${checkTest(t, a.insertMany(t).fn)}
            |
            |  ${a.get(t).map { g => checkTest(t, g.inner)}.getOrElse("")}
            |
            |  ${a.find(t).map { f => checkTest(t, f.inner)}.getOrElse("")}
            |
            |  ${checkTest(t, a.all(t).inner)}
            |
            |  ${checkTest(t, a.count(t).inner)}
            |
            |  ${a.baseMultiget(t).map(f => checkTest(t, f.fn)).getOrElse("")}
            |
            |  ${a.update(t).map { u => checkTest(t, u.inner) }.mkString("\n") }
            |}
         """.stripMargin

      File(
        a.targetPackage(t),
        a.targetObject(t) + "Spec.scala",
        contents,
        isTest = true
      )
    }

    tableFiles ++ testFiles
  }

  def getTypes(table: Table): Set[sql.Type] = {
    table.properties.flatMap {
      case sql.Column(_, sqlType, _) => List(sqlType)
      case _ => List.empty
    }.toSet
  }

  def genImports(table: Table): String = {
    val types = getTypes(table)

    if (types.contains(sql.JsonB)) {
      s"""import argonaut.{Json, Parse}
         |import org.postgresql.util.PGobject"""
    } else {
      ""
    }
  }

  def genMisc(table: Table): String = {
    val types = getTypes(table)

    if (types.contains(sql.JsonB)) {
s"""implicit val JsonMeta: doobie.imports.Meta[Json] =
  doobie.imports.Meta.other[PGobject]("jsonb").nxmap[Json](
    a => Parse.parse(a.getValue).leftMap[Json](sys.error).merge, // failure raises an exception
    a => new PGobject <| (_.setType("jsonb")) <| (_.setValue(a.nospaces))
  )"""
    } else {
      ""
    }
  }

  def genPkNewType(table: Table): String = {
    a.pkNewType(table).map { pk =>
      s"case class ${pk._2.symbol}(${pk._1.map(f => s"${f.scalaName}: ${f.scalaType.qualifiedSymbol}").mkString(", ")})"
    }.getOrElse("")
  }

  def genRowType(table: Table): String = {
    val row = a.rowNewType(table)
    s"case class ${row._2.symbol}(${row._1.map(f => s"${f.scalaName}: ${f.scalaType.qualifiedSymbol}").mkString(", ")})"
  }

  def genShapeType(table: Table): String = {
    val shape = a.rowShape(table)
    s"case class ${shape._2.symbol}(${shape._1.map(f => s"${f.scalaName}: ${f.scalaType.qualifiedSymbol}").mkString(", ")})"
  }

  def genInsert(table: Table): String = {
    val in = a.insert(table)
    ppFunctionDef(in.fn)
  }

  def genCreate(table: Table): String = {
    val create = a.create(table)
    ppFunctionDef(create.fn)
  }

  def ppFunctionDef(fn: FunctionDef): String = {
    val scope = fn.privatePkg.map { p => s"private[$p]" }.getOrElse("")
    val params = fn.params.map(f => s"${f.name}: ${f.`type`.qualifiedSymbol}").mkString(", ")

    s"""$scope def ${fn.name}($params): ${fn.returnType} = {
       |${fn.body}
       |}
     """.stripMargin

  }

  def genGet(table: Table): String = {
    a.get(table).map { get =>
      s"""${ppFunctionDef(get.inner)}
         |${ppFunctionDef(get.outer)}
       """.stripMargin
    }.getOrElse("")
  }

  def checkTest(table: Table, fn: FunctionDef): String = {
    val obj = a.targetObject(table)
    s"""check($obj.${fn.name}(${fn.params.map(_.`type`.qualifiedArb).mkString(", ")}))"""
  }

}
