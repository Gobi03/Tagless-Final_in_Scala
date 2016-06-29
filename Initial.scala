sealed abstract class Repr

case class IInt(n: Int) extends Repr
case class IAdd(a: Repr, b: Repr) extends Repr

val ini1 = IAdd(IInt(1), IInt(2))

def run(exp: Repr): Int = {
  exp match{
    case IInt(n) => n
    case IAdd(a, b) => run(a) + run(b)
  }
}

def printer(exp: Repr): String = {
  exp match{
    case IInt(n) => n.toString
    case IAdd(a, b) => printer(a) + " + " + printer(b)
  }
}
