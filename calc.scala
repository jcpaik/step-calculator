trait Expr {
  override def toString: String = 
    this match 
      case IntNum(n) => if n >= 0 then s"$n" else s"(-$n)"
      case Fraction(n, d) => if n >= 0 then s"$n/$d" else s"(-$n/$d)"
      case Add(e1, e2) => s"($e1 + $e2)"
      case Sub(e1, e2) => s"($e1 - $e2)"
      case Mul(e1, e2) => s"($e1 * $e2)"
      case Div(e1, e2) => s"($e1 / $e2)"
      // case Negate(e) => s"(-$e)"
}
trait Number extends Expr
case class IntNum(n: Int) extends Number
case class Fraction(n: Int, d: Int) extends Number {
  assert(d != 0)
}
trait Operator extends Expr
trait UnaryOperator extends Operator
// case class Negate(expr: Expr) extends UnaryOperator
trait BinaryOperator extends Operator
case class Add(left: Expr, right: Expr) extends BinaryOperator
case class Sub(left: Expr, right: Expr) extends BinaryOperator
case class Mul(left: Expr, right: Expr) extends BinaryOperator
case class Div(left: Expr, right: Expr) extends BinaryOperator

implicit def int2intNum(n: Int): IntNum = IntNum(n)

// TODO: use reflection
def head(op: Expr): (Expr, Expr) => Expr = (f1, f2) => {
  op match
    case Add(e1, e2) => Add(f1, f2)
    case Sub(e1, e2) => Sub(f1, f2)
    case Mul(e1, e2) => Mul(f1, f2)
    case Div(e1, e2) => Div(f1, f2)
}

object BinaryOperator {
  def unapply(e: Expr): Option[(Expr, Expr)] = 
    e match 
      case Add(e1, e2) => Some(e1, e2)
      case Sub(e1, e2) => Some(e1, e2)
      case Mul(e1, e2) => Some(e1, e2)
      case Div(e1, e2) => Some(e1, e2)
      case _ => None
}

object ZeroNum {
  def unapply(e: Expr): Boolean = 
    e match
      case IntNum(n) => n == 0
      case Fraction(n, d) => n == 0
      case _ => false
}

case class Step(desc: String, result: Any = None, substeps: List[Step] = List()) {
  def toString(offset: Int = 0): String = 
    " " * offset + desc + s"  => $result\n" + 
      substeps.map(s => s.toString(offset + 2)).mkString
  override def toString: String = toString(0)
}

abstract class Solution[+A] {
  def map[B](f: A => B): Solution[B]
  def flatMap[B](f: A => Solution[B]): Solution[B]
  def withFilter(f: A => Boolean): Solution[A]
  def handleFailureWith[B >: A](f: Any => Solution[B]): Solution[B]
}

case class Success[+A](result: A, steps: List[Step]) extends Solution[A] {
  def map[B](f: A => B): Solution[B] = Success(f(result), steps)
  def flatMap[B](f: A => Solution[B]): Solution[B] = 
    f(result) match 
      case Success(next_result, next_steps) =>
        Success(next_result, steps ::: next_steps)
      case Failure(reason) =>
        Failure(reason)
  def withFilter(test: A => Boolean): Solution[A] = 
    if test(result) then this else Failure("Test failed")
  def handleFailureWith[B >: A](f: Any => Solution[B]): Solution[B] = this
}

case class Failure[+A](reason: Any) extends Solution[A] {
  def map[B](f: A => B): Failure[B] = Failure(reason) 
  def flatMap[B](f: A => Solution[B]): Failure[B] = Failure(reason)
  def withFilter(f: A => Boolean): Failure[A] = this
  def handleFailureWith[B >: A](f: Any => Solution[B]): Solution[B] = f(reason)
}

implicit def title2solution(desc: String): Solution[Unit] =
  Success((), List(Step(desc)))

implicit class Title(val desc: String) {
  def returns[A](res: A): Solution[A] = 
    Success(res, List(Step(desc, res)))

  def substeps[A](sol: Solution[A]): Solution[A] = 
    sol match
      case Success(res, steps) => Success(res, List(Step(desc, res, steps)))
      case Failure(reason) => Failure(reason)
}

type Rule[A, B] = (A => Solution[B])
def repeat[A](f: Rule[A, A]): Rule[A, A] = cur => {
  for {
    nxt <- f(cur)
    res <- repeat(f)(nxt)
  } yield res
} handleFailureWith (_ => Success(cur, List()))

def tryOrElseList[A](l: List[Solution[A]]): Solution[A] =
  l match 
    case Nil => Failure("Used all lists")
    case hd :: tl => hd handleFailureWith {_ => tryOrElseList(tl)}

def tryOrElse[A](a: Solution[A]*): Solution[A] = 
  tryOrElseList[A](a.toList)

def calculate: Rule[Expr, Expr] = repeat(simplify)

def simplify(e: Expr): Solution[Expr] = 
  tryOrElse(evaluate(e), simplifySubexpr(e))

def evaluate(e: Expr): Solution[Expr] = 
  tryOrElse(addNums(e), subNums(e), mulNums(e), divNums(e))

def simplifySubexpr(e: Expr): Solution[Expr] = 
  e match
    case BinaryOperator(e1, e2) => tryOrElse(
      s"Evaluate first term $e1" substeps (for f1 <- simplify(e1) yield head(e)(f1, e2)),
      s"Evaluate second term $e2" substeps (for f2 <- simplify(e2) yield head(e)(e1, f2))
    )
    case _ => Failure("Not a binary operator")

def makeFraction(a: Int, factor: Int): Solution[Fraction] = 
  s"Make $a into a fraction with denominator $factor" returns 
    Fraction(a * factor, factor)

def multCommonFactor(a: Fraction, factor: Int): Solution[Fraction] =
  s"Multiply factor $factor to numerator and denominator of $a" returns 
    Fraction(factor * a.n, factor * a.d)

def addNums(e: Expr): Solution[Expr] = 
  e match 
    case Add(e, ZeroNum()) =>
      s"Adding zero keeps the number same" returns e
    case Add(ZeroNum(), e) =>
      s"Adding zero keeps the number same" returns e
    case Add(IntNum(n1), IntNum(n2)) => 
      s"Add two integers $n1 and $n2" returns IntNum(n1 + n2)
    case Add(f1 @ Fraction(n1, d1), IntNum(n2)) => {
      for {
        f2 <- makeFraction(n2, d1)
      } yield Add(f1, f2)
    }
    case Add(IntNum(n1), f2 @ Fraction(n2, d2)) => {
      for {
        f1 <- makeFraction(n1, d2)
      } yield Add(f1, f2)
    }
    case Add(f1 @ Fraction(n1, d1), f2 @ Fraction(n2, d2)) => 
      if d1 == d2 then
        s"Add numerator of $f1 and $f2" returns Fraction(n1 + n2, d1)
      else
        s"Make common factors of $f1 and $f2" substeps {
          for {
            g1 <- multCommonFactor(f1, d2)
            g2 <- multCommonFactor(f2, d1)
          } yield Add(g1, g2)
        }
    case Add(_, _) => 
      Failure("Don't know how to add further")
    case _ => 
      Failure("Not addition")

def subNums(e: Expr): Solution[Expr] = 
  e match 
    case Sub(e, ZeroNum()) =>
      s"Subtracting zero keeps the number same" returns e
    case Sub(IntNum(n1), IntNum(n2)) => 
      s"Subtract two integers $n1 and $n2" returns IntNum(n1 - n2)
    case Sub(f1 @ Fraction(n1, d1), IntNum(n2)) => {
      for {
        f2 <- makeFraction(n2, d1)
      } yield Sub(f1, f2)
    }
    case Sub(IntNum(n1), f2 @ Fraction(n2, d2)) => {
      for {
        f1 <- makeFraction(n1, d2)
      } yield Sub(f1, f2)
    }
    case Sub(f1 @ Fraction(n1, d1), f2 @ Fraction(n2, d2)) => 
      if d1 == d2 then
        s"Subtract numerator of $f1 and $f2" returns Fraction(n1 - n2, d1)
      else
        s"Make common factors of $f1 and $f2" substeps { for {
            g1 <- multCommonFactor(f1, d2)
            g2 <- multCommonFactor(f2, d1)
          } yield Sub(g1, g2) }
    case Sub(_, _) =>
      Failure("Don't know how to subtract further")
    case _ => 
      Failure("Not subtraction")

def mulNums(e: Expr): Solution[Expr] = 
  e match 
    // Multiplication by zero not yet implemented. See TODO.md
    case Mul(IntNum(n1), IntNum(n2)) => 
      s"Multiply two integers $n1 and $n2" returns IntNum(n1 * n2)
    case Mul(f1 @ Fraction(n1, d1), IntNum(n2)) => {
      s"Multiply $n2 to numerator of $f1" substeps { for {
        _ <- mulNums(Mul(n1, n2))
      } yield Fraction(n1 * n2, d1) } }
    case Mul(IntNum(n1), f2 @ Fraction(n2, d2)) =>
      s"Multiply $n1 to numerator of $f2" substeps { for {
        _ <- mulNums(Mul(n1, n2))
      } yield Fraction(n1 * n2, d2) }
    case Mul(f1 @ Fraction(n1, d1), f2 @ Fraction(n2, d2)) => 
      s"Multiply numerator and denominators of $f1 and $f2" substeps { for {
        _ <- mulNums(Mul(n1, n2))
        _ <- mulNums(Mul(d1, d2))
      } yield Fraction(n1 * n2, d1 * d2) }
    case Mul(_, _) =>
      Failure("Don't know how to multiply further")
    case _ =>
      Failure("Not multiplication")

def divNums(e: Expr): Solution[Expr] = 
  e match 
    case Div(_, ZeroNum()) => Failure("Division by zero")
    case Div(IntNum(n1), IntNum(n2)) => 
      if n2 == 0
        then Failure("Division by zero")
      else if n1 % n2 == 0
        then s"Divide two integers $n1 and $n2" returns IntNum(n1 / n2)
      else 
        s"Make fraction of $n1 and $n2" returns Fraction(n1, n2)
    case Div(f1 @ Fraction(n1, d1), IntNum(n2)) => {
      s"Multiply $n2 to denominator of $f1" substeps { for {
        _ <- mulNums(Mul(d1, n2))
      } yield Fraction(n1, d1 * n2) } }
    case Div(IntNum(n1), f2 @ Fraction(n2, d2)) =>
      s"Change to multiplication by switching numerator and denominator of $f2" returns
        Mul(n1, Fraction(d2, n2))
    case Mul(f1 @ Fraction(n1, d1), f2 @ Fraction(n2, d2)) => 
      s"Cross-multiply numerator and denominators of $f1 and $f2" substeps { for {
        _ <- mulNums(Mul(n1, d2))
        _ <- mulNums(Mul(n2, d1))
      } yield Fraction(n1 * d2, n2 * d1) }
    case Mul(_, _) =>
      Failure("Don't know how to divide further")
    case _ => 
      Failure("Not division")

def sol(e: Expr): String = {
  val calc = calculate(e)
  val sol = {
    calc match 
      case Success(_, sol) => sol.mkString
      case Failure(reason) => s"Failed: $reason"
  }
  val res: Option[Expr] = {
    calc match
      case Success(r, _) => Some(r)
      case Failure(_) => None
  }
  s"Input: $e\n\n$sol\n$res"
}

val ex1 = sol(Add(Add(0, 0), Add(3, 4)))
val ex2 = sol(Add(Add(Div(1, 2), Add(3, 4)), Add(4, 0)))
val ex3 = sol(Mul(Add(4, Div(1, 5)), Add(5, 5)))