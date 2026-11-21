package kyseliov.example

import kyseliov.*
object Main :
  // Type alias for our effect stack
  type AppStack = FxCons[[X] =>> Ask[String, X], FxCons[Log, FxNil]]

  // Polymorphic program: only requires Member constraints, completely agnostic to stack order
  def program[R](using
                 Member[[X] =>> Ask[String, X], R],
                 Member[Log, R]
                ): Eff[R, Int] =
    for
      user <- ask[String, R]
      _ <- log(s"Greeting $user")
      _ <- log("Computation complete")
    yield user.length

  @main def demo(): Unit =
    val concreteProgram: Eff[AppStack, Int] = program[AppStack]

    // Peel off Ask, then peel off Log, then extract the final pure value
    val step1: Eff[FxCons[Log, FxNil], Int] = runAsk("Alice")(concreteProgram)
    val step2: Eff[FxNil, (List[String], Int)] = runLog(step1)
    val (logs, length) = Eff.run(step2)

    println(s"Logs: $logs") // Logs: List(Greeting Alice, Computation complete)
    println(s"Result: $length") // Result: 5
