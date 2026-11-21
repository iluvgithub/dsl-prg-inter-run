package kyseliov.freer

import kyseliov.freer.Trampoline.Bounce
import munit.FunSuite
import org.scalatest.matchers.should.Matchers.*

class TestFree extends FunSuite:

  test("simple money transfer "):
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