package example.taglessfinal.domain
 
final case class Account(no: String, balance: BigDecimal)

// =============================================================================
// 1. ALGEBRA
//    In tagless final the algebra is a plain trait parameterised by the
//    effect type F[_]. There is no AST, no Free, no smart constructors:
//    the trait's methods ARE the vocabulary programs are written in.
// =============================================================================
trait AccountAlg[F[_]]:
  def credit(no: String, amt: BigDecimal): F[Account]
  def debit(no: String, amt: BigDecimal): F[Account]