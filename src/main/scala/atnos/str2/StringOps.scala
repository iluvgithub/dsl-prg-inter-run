package atnos.str2
//> using scala 3.3.4
//> using dep org.typelevel::cats-effect:3.5.7

import cats.{Applicative, Id, Monad}
import cats.effect.{IO, IOApp, Resource}
import cats.syntax.all.*

import com.sun.net.httpserver.{HttpExchange, HttpServer}
import java.net.{InetSocketAddress, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}

// ---------------------------------------------------------------------------
// 1. The algebra
// ---------------------------------------------------------------------------
trait StringOps[F[_]]:
  def toUpper(s: String): F[String]
  def toLower(s: String): F[String]
  def length(s: String): F[Int]
  def charFrequencies(s: String): F[Map[Char, Int]]

object StringOps:

  // -------------------------------------------------------------------------
  // 2a. Direct interpreter: pure computation, works in any Applicative
  // -------------------------------------------------------------------------
  def direct[F[_]: Applicative]: StringOps[F] = new StringOps[F]:
    def toUpper(s: String): F[String] = s.toUpperCase.pure[F]
    def toLower(s: String): F[String] = s.toLowerCase.pure[F]
    def length(s: String): F[Int]     = s.length.pure[F]
    def charFrequencies(s: String): F[Map[Char, Int]] =
      s.groupMapReduce(identity)(_ => 1)(_ + _).pure[F]

  // -------------------------------------------------------------------------
  // 2b. Remote interpreter: each operation is an HTTP POST, so F must be IO
  // -------------------------------------------------------------------------
  def remote(client: HttpClient, baseUri: URI): StringOps[IO] = new StringOps[IO]:

    private def post(path: String, body: String): IO[String] =
      val request = HttpRequest
        .newBuilder(baseUri.resolve(path))
        .POST(HttpRequest.BodyPublishers.ofString(body))
        .build()
      IO.fromCompletableFuture(
        IO(client.sendAsync(request, HttpResponse.BodyHandlers.ofString()))
      ).flatMap { resp =>
        if resp.statusCode == 200 then IO.pure(resp.body)
        else IO.raiseError(RuntimeException(s"HTTP ${resp.statusCode} on $path: ${resp.body}"))
      }

    def toUpper(s: String): IO[String] = post("/upper", s)
    def toLower(s: String): IO[String] = post("/lower", s)
    def length(s: String): IO[Int]     = post("/length", s).map(_.trim.toInt)
    def charFrequencies(s: String): IO[Map[Char, Int]] =
      // wire format: one "<charCode>:<count>" per line
      post("/frequencies", s).map { body =>
        body.linesIterator
          .filter(_.nonEmpty)
          .map { line =>
            val Array(code, count) = line.split(':')
            code.toInt.toChar -> count.toInt
          }
          .toMap
      }

// ---------------------------------------------------------------------------
// 3. Business logic: written once, against the algebra only
// ---------------------------------------------------------------------------
def report[F[_]: Monad](ops: StringOps[F])(s: String): F[String] =
  for
    up   <- ops.toUpper(s)
    low  <- ops.toLower(s)
    n    <- ops.length(s)
    freq <- ops.charFrequencies(s)
  yield s"upper=$up lower=$low length=$n freq=" +
    freq.toList.sorted.map((c, k) => s"'$c'=$k").mkString(" ")

// ---------------------------------------------------------------------------
// 4. A tiny local server playing the role of the remote service
//    (it reuses the direct interpreter, in Id)
// ---------------------------------------------------------------------------
val server: Resource[IO, URI] =
  Resource
    .make(IO {
      val srv = HttpServer.create(InetSocketAddress("localhost", 0), 0) // port 0 = any free port
      val ops = StringOps.direct[Id]

      def route(path: String)(f: String => String): Unit =
        srv.createContext(path, (ex: HttpExchange) => {
          val in  = String(ex.getRequestBody.readAllBytes, "UTF-8")
          val out = f(in).getBytes("UTF-8")
          ex.sendResponseHeaders(200, out.length)
          ex.getResponseBody.write(out)
          ex.close()
        })

      route("/upper")  (ops.toUpper)
      route("/lower")  (ops.toLower)
      route("/length") (s => ops.length(s).toString)
      route("/frequencies")(s =>
        ops.charFrequencies(s).map((c, k) => s"${c.toInt}:$k").mkString("\n"))

      srv.start()
      srv
    })(srv => IO(srv.stop(0)))
    .map(srv => URI.create(s"http://localhost:${srv.getAddress.getPort}"))
//> using scala 3.3.4
//> using dep org.atnos::eff-core:8.1.0
//> using dep org.typelevel::cats-effect:3.5.7

import cats.{Applicative, Id, Monad}
import cats.data.Reader
import cats.effect.{IO, IOApp, Resource}
import cats.syntax.all.*
import org.atnos.eff.*
import org.atnos.eff.reader.{ask, runReader}

// ... sections 1-4 from before: StringOps, direct, remote, report, server ...

// ---------------------------------------------------------------------------
// 5. Eff glue
// ---------------------------------------------------------------------------
type ReaderInput[A] = Reader[String, A]       // the effect: "an environment holding the input string"
type _input[R] = ReaderInput |= R             // "R can ask for the input string"
type _io[R]    = IO |= R                      // "R can run IO"

type Fx2Stack = Fx.fx2[ReaderInput, IO]       // Reader + IO
type Fx1Stack = Fx.fx1[ReaderInput]           // Reader only (pure)

// Interpreter A, lifted: the direct interpreter at F = Eff[R, *]
def directEff[R]: StringOps[[A] =>> Eff[R, A]] =
  StringOps.direct[[A] =>> Eff[R, A]](using Eff.EffMonad[R])

// Interpreter B, lifted: the remote (IO) interpreter, each call sent into the stack
def remoteEff[R: _io](ops: StringOps[IO]): StringOps[[A] =>> Eff[R, A]] =
  new StringOps[[A] =>> Eff[R, A]]:
    def toUpper(s: String)        = Eff.send[IO, R, String](ops.toUpper(s))
    def toLower(s: String)        = Eff.send[IO, R, String](ops.toLower(s))
    def length(s: String)         = Eff.send[IO, R, Int](ops.length(s))
    def charFrequencies(s: String) = Eff.send[IO, R, Map[Char, Int]](ops.charFrequencies(s))

// The program: the input is no longer an argument, it is asked from the Reader
def program[R: _input](ops: StringOps[[A] =>> Eff[R, A]]): Eff[R, String] =
  given Monad[[A] =>> Eff[R, A]] = Eff.EffMonad[R]
  for
    s <- ask[R, String]
    r <- report[[A] =>> Eff[R, A]](ops)(s)
  yield r

// ---------------------------------------------------------------------------
// 6. Run: handle Reader first, then deal with what is left
// ---------------------------------------------------------------------------
object Main extends IOApp.Simple:
  val input = "Hello, World"

  def run: IO[Unit] =
    server.use { uri =>
      val client = HttpClient.newHttpClient()

      // Direct: no IO in the stack at all, so the result is a plain String
      val a: String =
        Eff.run(runReader(input)(program[Fx1Stack](directEff[Fx1Stack])))

      // Remote: IO stays in the stack; after the Reader is handled, convert to IO
      val b: IO[String] =
        Eff.detach(
          runReader(input)(program[Fx2Stack](remoteEff[Fx2Stack](StringOps.remote(client, uri))))
        )

      for
        r <- b
        _ <- IO.println(s"direct: $a")
        _ <- IO.println(s"remote: $r")
        _ <- IO.println(s"same result: ${a == r}")
      yield ()
    }