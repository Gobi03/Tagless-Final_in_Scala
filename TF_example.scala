import scala.language.higherKinds

trait SYM {
  type Repr[_]

  def int(n: Int): Repr[Int]
  def add(x: Repr[Int])(y: Repr[Int]): Repr[Int]

  def lam[A, B](f: Repr[A] => Repr[B]): Repr[A => B]
  def app[A, B](f: Repr[A => B])(x: Repr[A]): Repr[B]
}

// evaluator
class Run extends SYM {
  type Repr[A] = A

  def int(n: Int) = n
  def add(x: Int)(y: Int) = x + y

  def lam[A, B](f: A => B) = f
  def app[A, B](f: A => B)(x: A) = f(x)
}

// example
def TF1(S: SYM) = {
  import S._
  add(int(1))(int(2))
}

val ar = TF1(new Run)

def TF2(S: SYM) = {
  import S._
  val f = lam((x: Repr[Int]) => (lam((y: Repr[Int]) => add(x)(y))))
  (x: Int) => ((y: Int) => app (app(f)(int(x))) (int(y)))
}
val fr = TF2(new Run)


/*
// prety-printer
class Print extends SYM {
  type Repr[A] = String

  def int(n: Int) = n.toString
  def add(x: String)(y: String) = x + "+" + y

  def lam[A, B](f: A => B) = f
  def app[A, B](f: A => B)(x: A) = f(x)
}
*/


// extending multi
trait MultiSYM extends SYM {
  def multi(x: Repr[Int])(y: Repr[Int]): Repr[Int]
}

class MultiRun extends Run with MultiSYM {
  def multi(x: Int)(y: Int) = x * y
}


def TF3(S: MultiSYM) = {
  import S._
  multi(int(3))(int(5))
}

val arm  = TF3(new MultiRun)
val arm2 = TF1(new MultiRun)
