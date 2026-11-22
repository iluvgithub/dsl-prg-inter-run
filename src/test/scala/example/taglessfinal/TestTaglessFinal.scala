package example.taglessfinal
 

//> using test.dep org.scalatest::scalatest:3.2.18

import cats.Monad
import example.taglessfinal.domain.Account
import example.taglessfinal.interpreter.Interpreters.{InMemoryAccounts, JdbcAccounts, Tx}
import example.taglessfinal.program.Program
import example.taglessfinal.runner.Runner.runTransactionally
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.sql.{Connection, DriverManager}
import java.util.UUID
import scala.util.{Success, Try, Using}

class TestTaglessFinal extends AnyFunSuite with Matchers:

  // ---------------------------------------------------------------------------
  // In-memory interpreter (F = Try)
  // ---------------------------------------------------------------------------
  private def newMem() =
    InMemoryAccounts(Map("a-1" -> BigDecimal(500), "a-2" -> BigDecimal(0)))

  private def transferMem(mem: InMemoryAccounts, from: String, to: String, amt: BigDecimal) =
    Program.transfer[Try](from, to, amt)(using summon[Monad[Try]], mem)

  test("in-memory: transfer moves money between accounts"):
    val mem = newMem()

    transferMem(mem, "a-1", "a-2", 100) shouldBe
      Success((Account("a-1", 400), Account("a-2", 100)))

    mem.snapshot shouldBe Map("a-1" -> BigDecimal(400), "a-2" -> BigDecimal(100))

  test("in-memory: failing credit does NOT roll back the debit"):
    val mem = newMem()

    val result = transferMem(mem, "a-1", "missing", 100)

    result.isFailure shouldBe true
    result.failed.get shouldBe a[NoSuchElementException]
    result.failed.get.getMessage shouldBe "Account missing not found"
    mem.snapshot shouldBe Map("a-1" -> BigDecimal(400), "a-2" -> BigDecimal(0))

  test("in-memory: insufficient funds fails and leaves balances untouched"):
    val mem = newMem()

    val result = transferMem(mem, "a-1", "a-2", 1000)

    result.failed.get shouldBe a[IllegalStateException]
    result.failed.get.getMessage shouldBe "Insufficient funds in a-1"
    mem.snapshot shouldBe Map("a-1" -> BigDecimal(500), "a-2" -> BigDecimal(0))

  // ---------------------------------------------------------------------------
  // JDBC interpreter (F = Tx), against a fresh H2 in-memory database per test
  // ---------------------------------------------------------------------------
  private final class Db:
    private val url = s"jdbc:h2:mem:${UUID.randomUUID()};DB_CLOSE_DELAY=-1"
    val open: () => Connection = () => DriverManager.getConnection(url)

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

  private def transferJdbc(from: String, to: String, amt: BigDecimal): Tx[(Account, Account)] =
    Program.transfer[Tx](from, to, amt)(using summon[Monad[Tx]], JdbcAccounts)

  test("jdbc: transfer commits and moves money between accounts"):
    val db = Db()

    transferJdbc("a-1", "a-2", 100).runTransactionally(db.open) shouldBe
      Success((Account("a-1", 400), Account("a-2", 100)))

    db.balances() shouldBe Map("a-1" -> BigDecimal(400), "a-2" -> BigDecimal(100))

  test("jdbc: failing credit rolls back the debit"):
    val db = Db()

    val result = transferJdbc("a-1", "missing", 100).runTransactionally(db.open)

    result.isFailure shouldBe true
    result.failed.get shouldBe a[NoSuchElementException]
    result.failed.get.getMessage shouldBe "Account missing not found"
    db.balances() shouldBe Map("a-1" -> BigDecimal(500), "a-2" -> BigDecimal(0))

  test("jdbc: insufficient funds fails and leaves balances untouched"):
    val db = Db()

    val result = transferJdbc("a-1", "a-2", 1000).runTransactionally(db.open)

    result.failed.get shouldBe a[IllegalStateException]
    result.failed.get.getMessage shouldBe "Insufficient funds in a-1"
    db.balances() shouldBe Map("a-1" -> BigDecimal(500), "a-2" -> BigDecimal(0))