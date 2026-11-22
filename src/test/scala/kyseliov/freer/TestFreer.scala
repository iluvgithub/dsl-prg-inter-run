package kyseliov.freer

import kyseliov.freer.EffectSyntax.*
import kyseliov.freer.SamplePrograms.*
import munit.FunSuite
import org.scalatest.matchers.should.Matchers.shouldBe

class TestFreer extends FunSuite:
  test(" multiple effects : ask, log, exception"):
    // arrange
    val ef: Eff[NoFx, Either[String, ((Int, Int), List[String])]] = runExc(runWriter(runState(5)(runReader(10)(program[Fx]))))
    // act
    val r1: Either[String, ((Int, Int), List[String])] = run(ef)
    // assert
    r1 shouldBe Right(((30, 15), List("env=10 state=5", "new state=15")))

  test(" multiple effects failure"):
    // arrange
    type Fx2 = StateOf[Int] :+: ExcOf[String] :+: NoFx
    val ef: Eff[NoFx, Either[String, (Int, Int)]] = runExc(runState(0)(failing[Fx2]))
    // act
    val r2: Either[String, (Int, Int)] = run(ef)
    // assert
    r2 shouldBe Left("boom")

  test(" multiple effects failure"):
    // arrange
    type Fx3 = ExcOf[String] :+: StateOf[Int] :+: NoFx
    val ef: Eff[NoFx, (Either[String, Int], Int)] = runState(0)(runExc(failing[Fx3]))
    // act
    val r2: (Either[String, Int], Int) = run(ef)
    // assert
    r2 shouldBe (Left("boom"), 1)

  test(" multiple effects failure"):
    // arrange
    val n = 200000
    val deep: Eff[ReaderOf[Int] :+: NoFx, Int] =
      (1 to n).foldLeft(ask[Int, ReaderOf[Int] :+: NoFx])((m, _) => m.flatMap(x => Pure(x + 1)))
    val ef: Eff[NoFx, Int] = runReader(0)(deep)
    // act
    val r4: Int = run(ef)
    // assert
    r4 shouldBe n
