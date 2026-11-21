package atnos.str

import cats.effect.{IO, IOApp, Resource}
import cats.syntax.all.*
import cats.{Applicative, Id, Monad}
import com.sun.net.httpserver.{HttpExchange, HttpServer}

import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.{InetSocketAddress, URI}

object StringTransform extends IOApp.Simple:

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
    def direct[F[_] : Applicative]: StringOps[F] = new StringOps[F]:
      def toUpper(s: String): F[String] = s.toUpperCase.pure[F]

      def toLower(s: String): F[String] = s.toLowerCase.pure[F]

      def length(s: String): F[Int] = s.length.pure[F]

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

      def length(s: String): IO[Int] = post("/length", s).map(_.trim.toInt)

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
  def report[F[_] : Monad](ops: StringOps[F])(s: String): F[String] =
    for
      up <- ops.toUpper(s)
      low <- ops.toLower(s)
      n <- ops.length(s)
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
            val in = String(ex.getRequestBody.readAllBytes, "UTF-8")
            val out = f(in).getBytes("UTF-8")
            ex.sendResponseHeaders(200, out.length)
            ex.getResponseBody.write(out)
            ex.close()
          })

        route("/upper")(ops.toUpper)
        route("/lower")(ops.toLower)
        route("/length")(s => ops.length(s).toString)
        route("/frequencies")(s =>
          ops.charFrequencies(s).map((c, k) => s"${c.toInt}:$k").mkString("\n"))

        srv.start()
        srv
      })(srv => IO(srv.stop(0)))
      .map(srv => URI.create(s"http://localhost:${srv.getAddress.getPort}"))

  // ---------------------------------------------------------------------------
  // 5. Run both interpreters on the same program
  // ---------------------------------------------------------------------------

  def run: IO[Unit] =
    server.use { uri =>
      val client = HttpClient.newHttpClient()
      for
        a <- report(StringOps.direct[IO])("Hello, World")
        b <- report(StringOps.remote(client, uri))("Hello, Worrld")
        _ <- IO.println(s"direct: $a")
        _ <- IO.println(s"remote: $b")
        _ <- IO.println(s"same result: ${a == b}")
      yield ()
    }