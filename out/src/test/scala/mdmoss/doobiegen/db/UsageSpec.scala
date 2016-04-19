package mdmoss.doobiegen.db

import doobie.imports._
import mdmoss.doobiegen.db.gen.Test_Table_With_Caps
import mdmoss.doobiegen.db.schema.gen.Test
import org.specs2.mutable.Specification

import scalaz.concurrent.Task

object UsageSpec extends Specification {

  val transactor = DriverManagerTransactor[Task]("org.postgresql.Driver", "jdbc:postgresql:gen", "test", "test")

  val work = for {
    test1 <- Test.insert(Some(1)).run
    test2 <- Test.create(Some(2))
  } yield ()


  "doobiegen methods" >> {

    "must run without errors when called correctly" >> {
      transactor.trans(work).run must_== (())
    }

    "Must escape database strings to avoid case issues" >> {
      val row = transactor.trans(Test_Table_With_Caps.create(None)).run
      transactor.trans(Test_Table_With_Caps.update(row)).attemptRun.isRight must_== true
    }

  }




}
