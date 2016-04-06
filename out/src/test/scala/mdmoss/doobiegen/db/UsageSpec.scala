package mdmoss.doobiegen.db

import doobie.imports._
import mdmoss.doobiegen.db.schema.gen.Test
import org.specs2.mutable.Specification

import scalaz.concurrent.Task

object UsageSpec extends Specification {

  val transactor = DriverManagerTransactor[Task]("org.postgresql.Driver", "jdbc:postgresql:gen", "test", "test")

  val work = for {
    test1 <- Test.insert(Some(1)).run
    test2 <- Test.create(Some(2))
  } yield ()


  "Doobiegen methods" >> {

    "Must run without errors when called correctly" >> {
      transactor.trans(work).run must_== (())
    }



  }




}
