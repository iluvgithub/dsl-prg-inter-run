package example
//> using scala 3.3.3
//> using dep org.typelevel::cats-free:2.10.0
//> using dep com.h2database:h2:2.2.224
//
// Run with:  scala-cli run FrDomain.scala
//
// Layering (each layer only depends on the ones above it):
//
//   1. ALGEBRA      what operations exist (types + signatures, no behaviour)
//   2. PROGRAM      business use cases composed from the algebra (pure values)
//   3. INTERPRETER  gives meaning to each algebra operation (in-memory / JDBC)
//   4. RUNNER       executes a program with an interpreter + handles
//                   cross-cutting concerns (transaction, connection lifecycle)

import example.interpreter.Interpreters.InMemoryInterpreter
import example.program.Program
import example.runner.Runner.{runInMemory, runTransactionally}

import java.sql.DriverManager
import scala.util.Using


// =============================================================================
// Demo: same program, two interpreters, two runners
// =============================================================================

object Main:
  @main def runDemo(): Unit =

    val program = Program.transfer("a-1", "a-2", 100)
    val broken = Program.transfer("a-1", "missing", 100) // debit ok, credit fails

    // ---- in-memory ----
    val mem = InMemoryInterpreter(Map("a-1" -> BigDecimal(500), "a-2" -> BigDecimal(0)))
    println(s"in-memory ok     : ${program.runInMemory(mem)}")
    println(s"in-memory state  : ${mem.snapshot}")
    println(s"in-memory broken : ${broken.runInMemory(mem)}")
    println(s"in-memory state  : ${mem.snapshot}  (debit NOT rolled back)")

    // ---- JDBC (H2 in-memory database) ----
    val url = "jdbc:h2:mem:demo;DB_CLOSE_DELAY=-1"
    val open = () => DriverManager.getConnection(url)

    Using.resource(open()): c =>
      val st = c.createStatement()
      st.execute("CREATE TABLE accounts(no VARCHAR PRIMARY KEY, balance DECIMAL(20,2))")
      st.execute("INSERT INTO accounts VALUES ('a-1', 500), ('a-2', 0)")

    def balances(): Map[String, BigDecimal] =
      Using.resource(open()): c =>
        Using.resource(c.createStatement().executeQuery("SELECT no, balance FROM accounts")): rs =>
          Iterator
            .continually(rs)
            .takeWhile(_.next())
            .map(r => r.getString("no") -> BigDecimal(r.getBigDecimal("balance")))
            .toMap

    println(s"jdbc ok          : ${program.runTransactionally(open)}")
    println(s"jdbc state       : ${balances()}")
    println(s"jdbc broken      : ${broken.runTransactionally(open)}")
    println(s"jdbc state       : ${balances()}  (debit rolled back)")
