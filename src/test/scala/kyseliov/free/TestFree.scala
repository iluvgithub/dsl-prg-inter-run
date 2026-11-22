package kyseliov.free

import cats.Monoid
import kyseliov.free.ReaderWriterSyntax.IT
import kyseliov.free.StateSyntax.FreeState
import kyseliov.free.Trampoline.Bounce
import munit.FunSuite
import org.scalatest.matchers.should.Matchers.*

class TestFree extends FunSuite:

  test("simple sum using trampoline"):
    // arrange
    val n = 300000

    def loop(i: Int, acc: Long): Bounce[Long] =
      if (i <= 0) Trampoline.done(acc)
      else Trampoline.more(() => loop(i - 1, acc + i))

    def sum(i: Int): Long = Trampoline.run(loop(i, 0L))

    // act
    val out = sum(n)
    // assert
    out shouldBe 1L * n * (n + 1) / 2

  test("simple sum using state"):

    // arrange
    def sum(i: Int): Long =
      def f0(t2: (Int, Long)): FreeState[Unit, Either[(Int, Long), Long]] =
        if (t2._1 <= 0) StateSyntax.pureState(t2._2).map(Right(_))
        else StateSyntax.pureState(t2).map({ case (u, v) => (u - 1, v + u) }).map(Left(_))

      StateSyntax.runState((), Free.freeMonad.tailRecM((i, 0L))(f0))._1

    val n = 300000
    // act
    val out = sum(n)
    // assert
    out shouldBe 1L * n * (n + 1) / 2


  test("simple zip with state"):
    // arrange
    val n = 100000
    val l = List.range(0, n)
    // act
    val zipped = StateSyntax.zipIndex(l)
    // assert
    val out: Long = zipped.map({ case (u, v) => u + v + 0L }).sum
    out shouldBe (n - 1L) * n

  test("simple sum with read write"):
    // arrange
    given Monoid[Long] with
      override def empty: Long = 0L

      override def combine(x: Long, y: Long): Long = x + y

    def sum(i: Int): Long =
      val it0: IT[Long, Long, Long] = Iterator.range(0, i).foldLeft(ReaderWriterSyntax.getIT[Long, Long])(
        (acc, i) =>
          for {
            y <- acc
            _ = ReaderWriterSyntax.pureIT(y + i)
          } yield y + i
      )
      ReaderWriterSyntax.runIt(0L, it0)._1
    val n = 50000
    // act
    val out = sum(n)
    // assert
    out shouldBe 1L * n * (n - 1) / 2

