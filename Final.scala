type Repr = Int

def int(n: Int): Repr = n
def neg(a: Repr): Repr = -(a)
def add(a: Repr, b: Repr): Repr = a + b

// 1 + (5 - 3)
val fi1 = add(int(1), add(int(5), neg(int(3))))

def multi(a: Repr, b: Repr): Repr = a * b
val fi2 = multi(int(2), fi1)


type ReprP = String

def int(n: Int): ReprP = n.toString
def neg(a: ReprP): ReprP = "(-" + a + ")"
def add(a: ReprP, b: ReprP): ReprP = "(" + a + " + " + b + ")"

val fi1P = add(int(1), add(int(5), neg(int(3))))

// -(-(3 + (-(-(-1)))))
val fi2P = neg(neg(add(int(3), neg(neg(neg(int(1)))))))


// // optimization
// sealed abstract class Ctx
// case class Pos() extends Ctx
// case class Neg() extends Ctx

// type Repr = (Ctx => Repr)
