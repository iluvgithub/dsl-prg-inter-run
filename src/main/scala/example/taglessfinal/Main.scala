package example.taglessfinal

//> using scala 3.3.3
//> using dep org.typelevel::cats-core:2.10.0
//> using dep com.h2database:h2:2.2.224
//
// Run with:  scala-cli run .
//
// Layering:
//   1. ALGEBRA      trait AccountAlg[F[_]]
//   2. PROGRAM      use cases polymorphic in F: Monad[F] + AccountAlg[F]
//   3. INTERPRETER  an AccountAlg instance per target F (Try / Tx)
//   4. RUNNER       supplies the connection + transaction for F = Tx

import cats.Monad
import example.taglessfinal.interpreter.Interpreters.{InMemoryAccounts, JdbcAccounts, Tx}
import example.taglessfinal.program.Program
import example.taglessfinal.runner.Runner.runTransactionally

import java.sql.DriverManager
import scala.util.{Try, Using}

object Main:
  @main def runDemo(): Unit =

    // ---- in-memory: F = Try, the program IS the result ----
    val mem = InMemoryAccounts(Map("a-1" -> BigDecimal(500), "a-2" -> BigDecimal(0)))

    println(s"in-memory ok     : ${Program.transfer[Try]("a-1", "a-2", 100)(using summon, mem)}")
    println(s"in-memory state  : ${mem.snapshot}")
    println(s"in-memory broken : ${Program.transfer[Try]("a-1", "missing", 100)(using summon, mem)}")
    println(s"in-memory state  : ${mem.snapshot}  (debit NOT rolled back)")

    // ---- JDBC (H2 in-memory database): F = Tx ----
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

    val monadTx: Monad[Tx] = summon[Monad[Tx]]
    val ok     = Program.transfer[Tx]("a-1", "a-2", 100)(using monadTx, JdbcAccounts)
    val broken = Program.transfer[Tx]("a-1", "missing", 100)(using monadTx, JdbcAccounts)

    println(s"jdbc ok          : ${ok.runTransactionally(open)}")
    println(s"jdbc state       : ${balances()}")
    println(s"jdbc broken      : ${broken.runTransactionally(open)}")
    println(s"jdbc state       : ${balances()}  (debit rolled back)")