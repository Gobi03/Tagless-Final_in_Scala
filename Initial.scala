sealed abstract class Repr

case class IInt(n: Int) extends Repr
case class INeg(a: Repr) extends Repr
case class IAdd(a: Repr, b: Repr) extends Repr

// 1 + (5 - 3)
val ini1 = IAdd(IInt(1), IAdd(IInt(5), INeg(IInt(3))))

def run(exp: Repr): Int = {
  exp match{
    case IInt(n) => n
    case INeg(a) => -(run(a))
    case IAdd(a, b) => run(a) + run(b)
  }
}

def printer(exp: Repr): String = {
  exp match{
    case IInt(n) => n.toString
    case INeg(a) => "(-" + printer(a) + ")"
    case IAdd(a, b) => "(" + printer(a) + " + " + printer(b) + ")"
  }
}

run(ini1)
printer(ini1)

// -(-(3 + (-(-(-1)))))
val ini2 = INeg(INeg(IAdd(IInt(3), INeg(INeg(INeg(IInt(1)))))))

run(ini2)
printer(ini2)

def opt(exp: Repr): Repr = {
  exp match{
    case IInt(_) => exp
    case INeg(INeg(e)) => opt(e)
    case INeg(e) => INeg(opt(e))
    case IAdd(a, b) => IAdd(opt(a), opt(b))
  }
}

run(opt(ini2))
printer(opt(ini2))
