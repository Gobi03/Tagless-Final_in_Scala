trait SYM {
  type Repr

  def int(n: Int): Repr
  def neg(e: Repr): Repr
  def add(a: Repr)(b: Repr): Repr
}


// evaluator
class Run extends SYM {
  type Repr = Int

  def int(n: Int) = n
  def neg(e: Repr) = -e
  def add(a: Int)(b: Int) = a + b
}


// example
def TF1(S: SYM) = {
  import S._
  add(int(1))(int(2))
}

val ar = TF1(new Run)

// prety-printer
class Print extends SYM {
  type Repr = String

  def int(n: Int) = n.toString
  def neg(e: Repr) = "-" + e
  def add(x: String)(y: String) = "(" + x + " + " + y + ")"
}

val ap = TF1(new Print)


// extending multi
trait MultiSYM extends SYM {
  def multi(x: Repr)(y: Repr): Repr
}

class MultiRun extends Run with MultiSYM {
  def multi(x: Int)(y: Int) = x * y
}

// 3 * (2 + 3)
def TF2(S: MultiSYM) = {
  import S._
  multi(int(3))(add(int(2))(int(3)))
}

val br  = TF2(new MultiRun)
val ar2 = TF1(new MultiRun)


// // optimization
// sealed abstract class Ctx
// case class Pos() extends Ctx
// case class Neg() extends Ctx

// class Optimizer extends SYM {
//   type Repr = (Ctx => Repr)

//   def int(n: Int)(ctx: Ctx) =
//     ctx match {
//       case Pos => int(n)
//       case Neg => neg(int(n))
//     }
//   def neg(e: Repr)(ctx: Ctx) =
//     ctx match {
//       case Pos => e Neg
//       case Neg => e Pos
//     }
//   def add(a: Int)(b: Int)(ctx: Ctx) = add(a ctx)(b ctx)
// }
