package mdmoss.doobiegen.output

import java.io.FileWriter
import java.nio.file.{Path, Paths, Files}

object SourceWriter {

  /**
    * Write given jvm files to appropriate locations.
    *
    * @param sourceRoot should be src/ directory of project
    */
  def write(sourceRoot: Path, files: Seq[File]): Unit = {
    files.foreach { f =>
      val mainOrTest = if (f.isTest) "test" else "main"

      val destDir = Paths.get(sourceRoot.toString, List(mainOrTest, "scala") ++ f.`package`.split('.'):_*)
      Files.createDirectories(destDir)

      val destFile = Paths.get(destDir.toString, f.name)
      Files.deleteIfExists(destFile)

      new FileWriter(destFile.toAbsolutePath.toString) { w => w.write(f.contents); w.close() }
    }
  }

  def main(args: Array[String]) {

    val files = Seq(
      File("a.b.c", "Test.scala", "Wah", true),
      File("b.c.a", "Test2.scala", "Woh", false)
    )

    write(Paths.get(""), files)
  }
}
