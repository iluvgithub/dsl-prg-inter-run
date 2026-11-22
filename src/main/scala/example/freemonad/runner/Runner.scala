package example.freemonad.runner

import example.freemonad.interpreter.Interpreters.{InMemoryInterpreter, JdbcInterpreter}

import java.sql.Connection
import scala.util.{Failure, Success, Try}

// =============================================================================
// 4. RUNNERS
//    Take a whole program, pick an interpreter, execute it, and handle
//    concerns that wrap the entire run (here: connection + transaction).
//    This is the only layer that touches the outside world's lifecycle.
//    Exposed as extension methods so a program reads as `program.runXxx(...)`.
// =============================================================================
object Runner:

  import example.freemonad.domain.*

  extension [A](program: AccountF[A])

    // Run against the in-memory interpreter: just fold the program.
    def runInMemory(interp: InMemoryInterpreter): Try[A] =
      program.foldMap(interp)

    // Run against JDBC: open connection, begin tx, interpret, commit or
    // rollback, close. The program stays atomic without knowing it.
    def runTransactionally(open: () => Connection): Try[A] =
      Try(open()).flatMap: conn =>
        val result: Try[A] =
          Try(conn.setAutoCommit(false))
            .flatMap(_ => program.foldMap(JdbcInterpreter).run(conn))

        val finish: Try[Unit] = result match
          case Success(_) => Try(conn.commit())
          case Failure(_) => Try(conn.rollback())

        Try(conn.close())

        finish.flatMap(_ => result)
