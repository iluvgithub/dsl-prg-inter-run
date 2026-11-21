package example.interpreter

import cats.data.Kleisli
import cats.~>
import example.domain.AccountOp.{Credit, Debit}

import java.sql.Connection
import scala.collection.mutable
import scala.util.{Failure, Success, Try, Using}

// =============================================================================
// 3. INTERPRETERS
//    Natural transformations AccountOp ~> M. They give meaning to ONE
//    operation at a time. They know nothing about programs, sequencing or
//    transactions.
// =============================================================================
object Interpreters:
  import example.domain.*

  // ---- 3a. In-memory interpreter (target monad: Try) -----------------------
  //  Handy for tests. Note: no rollback, each operation is applied immediately.
  final class InMemoryInterpreter(initial: Map[String, BigDecimal]) extends (AccountOp ~> Try):

    private val store = mutable.Map.from(initial)

    private def find(no: String): Try[Account] =
      store.get(no) match
        case Some(b) => Success(Account(no, b))
        case None    => Failure(new NoSuchElementException(s"Account $no not found"))

    private def save(a: Account): Account =
      store.update(a.no, a.balance)
      a

    def apply[A](op: AccountOp[A]): Try[A] = op match
      case Credit(no, amt) =>
        find(no).map(a => save(a.copy(balance = a.balance + amt)))

      case Debit(no, amt) =>
        find(no).flatMap: a =>
          if a.balance < amt then
            Failure(IllegalStateException(s"Insufficient funds in $no"))
          else Success(save(a.copy(balance = a.balance - amt)))

    def snapshot: Map[String, BigDecimal] = store.toMap

  // ---- 3b. JDBC interpreter (target monad: Tx = Connection => Try[A]) ------
  //  The connection is NOT created here: the interpreter only describes
  //  "given a connection, do this". Connection/transaction lifecycle is the
  //  runner's job.
  type Tx = [A] =>> Kleisli[Try, Connection, A]

  object JdbcInterpreter extends (AccountOp ~> Tx):

    private def load(conn: Connection, no: String): Try[Account] =
      Using(conn.prepareStatement("SELECT balance FROM accounts WHERE no = ? FOR UPDATE")): ps =>
        ps.setString(1, no)
        Using.resource(ps.executeQuery()): rs =>
          if rs.next() then Account(no, BigDecimal(rs.getBigDecimal("balance")))
          else throw new NoSuchElementException(s"Account $no not found")

    private def update(conn: Connection, a: Account): Try[Account] =
      Using(conn.prepareStatement("UPDATE accounts SET balance = ? WHERE no = ?")): ps =>
        ps.setBigDecimal(1, a.balance.bigDecimal)
        ps.setString(2, a.no)
        ps.executeUpdate()
        a

    def apply[A](op: AccountOp[A]): Tx[A] =
      Kleisli[Try, Connection, A]: conn =>
        op match
          case Credit(no, amt) =>
            load(conn, no).flatMap(a => update(conn, a.copy(balance = a.balance + amt)))

          case Debit(no, amt) =>
            load(conn, no).flatMap: a =>
              if a.balance < amt then
                Failure(IllegalStateException(s"Insufficient funds in $no"))
              else update(conn, a.copy(balance = a.balance - amt))
