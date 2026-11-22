package example.taglessfinal.program

import cats.Monad
import cats.syntax.all.*
import example.taglessfinal.domain.{Account, AccountAlg}

// =============================================================================
// 2. PROGRAM
//    Business use cases are polymorphic in F. They only require:
//      - Monad[F]       to sequence steps (this replaces Free's built-in monad)
//      - AccountAlg[F]  to use the domain operations
//    The "interpreter" is chosen by whoever picks F and supplies the instance.
// =============================================================================
object Program:

  def transfer[F[_]: Monad](from: String, to: String, amt: BigDecimal)(using
                                                                       accounts: AccountAlg[F]
  ): F[(Account, Account)] =
    for
      f <- accounts.debit(from, amt)
      t <- accounts.credit(to, amt)
    yield (f, t)
