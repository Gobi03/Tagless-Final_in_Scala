type Repr = Int

def int(n: Int): Repr = n
def add(a: Repr, b: Int): Repr = a + b

val fi1 = add(int(1), int(2))

def multi(a: Repr, b: Int): Repr = a * b
val fi2 = multi(int(2), fi1)
