package example

import example.domain.{Account, AccountF}
import example.interpreter.Interpreters.{InMemoryInterpreter, JdbcInterpreter}
import example.program.Program
import example.runner.Runner._
import munit.FunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.shouldBe

import java.sql.{Connection, DriverManager}
import scala.util.{Try, Using}

class TestExample extends FunSuite:

  test("simple money transfer "):
    val accounts = "a-1" -> BigDecimal(500) :: "a-2" -> BigDecimal(0) :: Nil
    val memInterpreter: InMemoryInterpreter = InMemoryInterpreter(accounts.toMap)

    val supplier: AccountF[(Account, Account)] =>  Try[(Account, Account)] =
      _.runInMemory(memInterpreter)
    runTestOk(supplier)


  test("jdbc money transfer "):
    val url = "jdbc:h2:mem:demo;DB_CLOSE_DELAY=-1"
    val open: () => Connection = () => DriverManager.getConnection(url)

    Using.resource(open()): c =>
      val st = c.createStatement()
      st.execute("CREATE TABLE accounts(no VARCHAR PRIMARY KEY, balance DECIMAL(20,2))")
      st.execute("INSERT INTO accounts VALUES ('a-1', 500), ('a-2', 0)")

    Using.resource(open()): c =>
      val supplier: AccountF[(Account, Account)] =>  Try[(Account, Account)] =
         _. runTransactionally(open)

      runTestOk(supplier)


  def runTestOk(supplier: AccountF[(Account, Account)] =>  Try[(Account, Account)] ) :Unit=
      // arrange
    val program: AccountF[(Account, Account)] = Program.transfer("a-1", "a-2", 100)
    // act
    val out: Try[(Account, Account)] = supplier(program)
    // assert
    out.isSuccess shouldBe true
    val (from: Account, to: Account) = out.get
    from shouldBe Account("a-1", 400)
    to shouldBe Account("a-2", 100)



