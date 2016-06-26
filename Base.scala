import scala.language.higherKinds

trait SYM {
  type Repr[A]

  def int(n: Int): Repr[Int]
  def add(x: Repr[Int])(y: Repr[Int]): Repr[Int]
}


// evaluator
class Run extends SYM {
  type Repr[A] = A

  def int(n: Int) = n
  def add(x: Int)(y: Int) = x + y
}


// example
def TF1(S: SYM) = {
  import S._
  add(int(1))(int(2))
}

val ar = TF1(new Run)

// prety-printer
class Print extends SYM {
  type Repr[A] = String

  def int(n: Int) = n.toString
  def add(x: String)(y: String) = "(" + x + " + " + y + ")"
}

val ap = TF1(new Print)


// extending multi
trait MultiSYM extends SYM {
  def multi(x: Repr[Int])(y: Repr[Int]): Repr[Int]
}

class MultiRun extends Run with MultiSYM {
  def multi(x: Int)(y: Int) = x * y
}

def TF2(S: MultiSYM) = {
  import S._
  multi(int(3))(int(5))
}

val br  = TF2(new MultiRun)
val ar2 = TF1(new MultiRun)
