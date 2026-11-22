package example.freemonad.program

import example.freemonad.domain.{Account, AccountF, credit, debit}

// =============================================================================
// 2. PROGRAM
//    Business use cases written ONLY in terms of the algebra.
//    A program is a plain value: nothing executes, no database, no map, no
//    transaction is mentioned. The same program runs against any interpreter.
// =============================================================================
object Program:
  import example.freemonad.domain.*

  def transfer(from: String, to: String, amt: BigDecimal): AccountF[(Account, Account)] =
    for
      f <- debit(from, amt)
      t <- credit(to, amt)
    yield (f, t)
