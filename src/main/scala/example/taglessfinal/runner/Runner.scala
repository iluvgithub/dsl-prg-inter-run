package example.taglessfinal.runner

import example.taglessfinal.interpreter.Interpreters.Tx

import java.sql.Connection
import scala.util.{Failure, Success, Try}

// =============================================================================
// 4. RUNNERS
//    With F = Try the program is already a result, so no runner is needed
//    (that is the main difference with Free: no foldMap step).
//    With F = Tx the program is still a function Connection => Try[A], so the
//    runner supplies the connection and handles the transaction around it.
// =============================================================================
object Runner:

  extension [A](program: Tx[A])

    def runTransactionally(open: () => Connection): Try[A] =
      Try(open()).flatMap: conn =>
        val result: Try[A] =
          Try(conn.setAutoCommit(false))
            .flatMap(_ => program.run(conn))

        val finish: Try[Unit] = result match
          case Success(_) => Try(conn.commit())
          case Failure(_) => Try(conn.rollback())

        Try(conn.close())

        finish.flatMap(_ => result)