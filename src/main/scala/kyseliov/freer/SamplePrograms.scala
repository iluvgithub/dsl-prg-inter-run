package kyseliov.freer

import kyseliov.freer.EffectSyntax._

object SamplePrograms:


  type Fx = ReaderOf[Int] :+: StateOf[Int] :+: WriterOf[String] :+: ExcOf[String] :+: NoFx

  /** Written against *constraints* on R, not against a concrete stack. */
  def program[R <: Effects](using
                            Member[ReaderOf[Int], R], Member[StateOf[Int], R], Member[WriterOf[String], R]): Eff[R, Int] =
    for
      e <- ask[Int, R]
      s <- get[Int, R]
      _ <- tell[String, R](s"env=$e state=$s")
      _ <- put[Int, R](s + e)
      s2 <- get[Int, R]
      _ <- tell[String, R](s"new state=$s2")
    yield s2 * 2


  def failing[R <: Effects](using Member[StateOf[Int], R], Member[ExcOf[String], R]): Eff[R, Int] =
    for
      _ <- put[Int, R](1)
      _ <- throwError[String, R, Unit]("boom")
      _ <- put[Int, R](2) // never reached
      s <- get[Int, R]
    yield s
