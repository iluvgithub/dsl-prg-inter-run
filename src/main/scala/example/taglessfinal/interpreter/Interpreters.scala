package example.taglessfinal.interpreter

import cats.data.Kleisli
import example.taglessfinal.domain.{Account, AccountAlg}

import java.sql.Connection
import scala.collection.mutable
import scala.util.{Failure, Success, Try, Using}

// =============================================================================
// 3. INTERPRETERS
//    Instances of the algebra for a concrete F. No natural transformation and
//    no foldMap: "interpreting" is simply implementing the trait.
// =============================================================================
object Interpreters:

  // ---- 3a. In-memory instance (F = Try) ------------------------------------
  //  Handy for tests. Note: no rollback, each operation is applied immediately.
  final class InMemoryAccounts(initial: Map[String, BigDecimal])
    extends AccountAlg[Try]:

    private val store = mutable.Map.from(initial)

    private def find(no: String): Try[Account] =
      store.get(no) match
        case Some(b) => Success(Account(no, b))
        case None    => Failure(new NoSuchElementException(s"Account $no not found"))

    private def save(a: Account): Account =
      store.update(a.no, a.balance)
      a

    def credit(no: String, amt: BigDecimal): Try[Account] =
      find(no).map(a => save(a.copy(balance = a.balance + amt)))

    def debit(no: String, amt: BigDecimal): Try[Account] =
      find(no).flatMap: a =>
        if a.balance < amt then Failure(IllegalStateException(s"Insufficient funds in $no"))
        else Success(save(a.copy(balance = a.balance - amt)))

    def snapshot: Map[String, BigDecimal] = store.toMap

  // ---- 3b. JDBC instance (F = Tx = Connection => Try[A]) -------------------
  //  The connection is NOT created here; each operation is a description of
  //  "given a connection, do this". Lifecycle is the runner's job.
  type Tx = [A] =>> Kleisli[Try, Connection, A]

  object JdbcAccounts extends AccountAlg[Tx]:

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

    def credit(no: String, amt: BigDecimal): Tx[Account] =
      Kleisli[Try, Connection, Account]: conn =>
        load(conn, no).flatMap(a => update(conn, a.copy(balance = a.balance + amt)))

    def debit(no: String, amt: BigDecimal): Tx[Account] =
      Kleisli[Try, Connection, Account]: conn =>
        load(conn, no).flatMap: a =>
          if a.balance < amt then Failure(IllegalStateException(s"Insufficient funds in $no"))
          else update(conn, a.copy(balance = a.balance - amt))