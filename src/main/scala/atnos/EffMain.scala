package atnos

//> using scala 3.3.4
//> using dep org.atnos::eff-core:8.1.0
// https://okmij.org/ftp/Haskell/extensible/more.pdf
// https://aistudio.google.com/apps/b7def981-79a0-405b-a65d-e4f8d713dbf3?showPreview=true&showAssistant=true

//> using scala 3.3.4
//> using dep org.atnos::eff-core:8.1.0

//> using scala 3.3.4
//> using dep org.atnos::eff-core:8.1.0

//> using scala 3.3.4
//> using dep org.atnos::eff-core:8.1.0
//> using dep org.typelevel::cats-effect:3.5.7

import cats.MonadError
import cats.data.{Reader, Writer}
import cats.effect.{IO, IOApp}
import org.atnos.eff.*
import org.atnos.eff.either.{catchLeft, fromEither, left, runEither}
import org.atnos.eff.reader.{ask, runReader}
import org.atnos.eff.writer.{runWriter, tell}

import scala.util.chaining.*

// ---------------------------------------------------------------------------
// 1. Domain types & Effect type constructors (native Scala 3 type lambdas)
// ---------------------------------------------------------------------------
final case class AppConfig(currency: String, maxBudget: Double, fallbackRate: Double)

enum AppError:
  case NetworkFailure(msg: String)
  case BudgetExceeded(requested: Double, max: Double)

type ReaderConfig[A] = Reader[AppConfig, A]
type WriterLog[A] = Writer[String, A]
type EitherError[A] = Either[AppError, A]

// Parametric row constraints (corresponding to `Member` in more.pdf):
// - `|=` (`MemberIn`) permits injecting/sending an effect into row R.
// - `Member` additionally permits inspecting/handling the effect in R (e.g. catchLeft).
type _config[R] = ReaderConfig |= R
type _log[R] = WriterLog |= R
type _errIn[R] = EitherError |= R
type _err[R] = Member[EitherError, R]
type _io[R] = IO |= R

def fromIO[R: _io, A](io: IO[A]): Eff[R, A] =
  Eff.send[IO, R, A](io)

// ---------------------------------------------------------------------------
// 2. Bridging cats.MonadError to an Eff row containing EitherError
// ---------------------------------------------------------------------------
object EffInstances:
  given effMonadError[R, E](using Member[[A] =>> Either[E, A], R]): MonadError[[A] =>> Eff[R, A], E] with
    private val M = Eff.EffMonad[R]

    export M.{pure, flatMap, tailRecM}

    def raiseError[A](e: E): Eff[R, A] = left[R, E, A](e)

    def handleErrorWith[A](fa: Eff[R, A])(f: E => Eff[R, A]): Eff[R, A] = catchLeft(fa)(f)

// ---------------------------------------------------------------------------
// 3. Modular sub-programs with minimal, independent effect rows
// ---------------------------------------------------------------------------

/** Uses ONLY Reader + Writer. Unaware of IO or Errors. */
def readBudgetLimit[R: _config : _log]: Eff[R, Double] =
  for
    cfg <- ask[R, AppConfig]
    _ <- tell[R, String](s"[Reader] Loaded config for ${cfg.currency}, maxBudget=${cfg.maxBudget}")
  yield cfg.maxBudget

/** Uses ONLY Cats Effect IO + Writer + EitherError injection. */
def fetchExchangeRate[R: _io : _log : _errIn](currency: String): Eff[R, Double] =
  for
    _ <- tell[R, String](s"[IO] Fetching FX rate for $currency...")
    raw <- fromIO(IO {
      if currency == "BAD" then Left(AppError.NetworkFailure("FX service rejected currency"))
      else Right(1.12)
    })
    rate <- fromEither[R, AppError, Double](raw)
    _ <- tell[R, String](s"[IO] Received rate=$rate")
  yield rate

/** Generic tagless-final validation using cats.MonadError[F, AppError]. */
def validateAmount[F[_]](amount: Double, max: Double)(using ME: MonadError[F, AppError]): F[Double] =
  if amount <= max then ME.pure(amount)
  else ME.raiseError(AppError.BudgetExceeded(amount, max))

// ---------------------------------------------------------------------------
// 4. Cumulating & composing all effects over a parametric row R
// ---------------------------------------------------------------------------
def quoteWorkflow[R: _config : _log : _err : _io](
                                                   requestedUnits: Double
                                                 ): Eff[R, Double] =
  import EffInstances.given

  for
    maxBudget <- readBudgetLimit[R]
    currency <- ask[R, AppConfig].map(_.currency)

    // Scoped error handling (`handle_relay` in more.pdf):
    // Explicit `(err: AppError)` fixes E = AppError for MemberInOut resolution.
    rate <- catchLeft(fetchExchangeRate[R](currency)) { (err: AppError) =>
      err match
        case AppError.NetworkFailure(reason) =>
          for
            fallback <- ask[R, AppConfig].map(_.fallbackRate)
            _ <- tell[R, String](s"[Recover] '$reason' -> using fallbackRate=$fallback")
          yield fallback
        case other =>
          left[R, AppError, Double](other)
    }

    total = requestedUnits * rate

    // Instantiate MonadError[F, AppError] with F = [A] =>> Eff[R, A]
    approved <- validateAmount[[A] =>> Eff[R, A]](total, maxBudget)
    _ <- tell[R, String](s"[Done] Approved quote: $approved")
  yield approved

// ---------------------------------------------------------------------------
// 5. Concrete effect stack & demonstrating handler-order semantics with IOApp
// ---------------------------------------------------------------------------
type AppStack = Fx.fx4[ReaderConfig, WriterLog, EitherError, IO]

object RunExtensibleEffects extends IOApp.Simple:
  val okConfig = AppConfig(currency = "BAD", maxBudget = 100.0, fallbackRate = 1.05)
  val failingConfig = AppConfig(currency = "EUR", maxBudget = 50.0, fallbackRate = 1.05)

  val recoveredProgram:  Eff[AppStack, Double] = quoteWorkflow[AppStack](requestedUnits = 80.0)
  val failingProgram:    Eff[AppStack, Double] = quoteWorkflow[AppStack](requestedUnits = 100.0)

  val t1: Eff[AppStack, Double] = recoveredProgram
  val t2: Eff[Fx3[WriterLog, EitherError, IO], Double] = t1.pipe(runReader(okConfig))
  val t3: Eff[Fx2[WriterLog, IO], Either[AppError, Double]] = t2.pipe(runEither)
  val t4: Eff[Fx1[IO], (Either[AppError, Double], List[String])] = t3.pipe(runWriter)
  val q1: IO[(Either[AppError, Double], List[String])] = Eff.detach(t4)

  /**
   *
   * def runEither[R, U, E, A](effect: Eff[R, A])(using Member.Aux[Either[E, *], R, U]):
   * Eff[U, Either[E, A]] =
   * interpretEither(effect)(cats.instances.either.catsStdInstancesForEither[E])
   */
  // Case 1: NetworkFailure is caught mid-flight via catchLeft; validation passes.
  val res1: IO[(Either[AppError, Double], List[String])] =
    Eff.detach(
      recoveredProgram
        .pipe(runReader(okConfig))
        .pipe(runEither)
        .pipe(runWriter)
    )

  // Case 2a: runEither BEFORE runWriter -> logs written prior to failure are PRESERVED.
  val keepLogsOnFailure: IO[(Either[AppError, Double], List[String])] =
    Eff.detach(
      failingProgram
        .pipe(runReader(failingConfig))
        .pipe(runEither)
        .pipe(runWriter)
    )

  // Case 2b: runWriter BEFORE runEither -> failure short-circuits Writer, logs are DISCARDED.
  val discardLogsOnFailure: IO[Either[AppError, (Double, List[String])]] =
    Eff.detach(
      failingProgram
        .pipe(runReader(failingConfig))
        .pipe(runWriter)
        .pipe(runEither)
    )

  def run: IO[Unit] =
    for
      r1 <- res1
      _ <- IO.println(s"Case 1 (recovered): $r1")
      r2 <- keepLogsOnFailure
      _ <- IO.println(s"Case 2a (Either inside Writer - logs kept): $r2")
      r3 <- discardLogsOnFailure
      _ <- IO.println(s"Case 2b (Writer inside Either - logs discarded): $r3")
    yield ()