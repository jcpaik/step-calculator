trait Expr

// TODO: currently only supports binary operators
// That is, (1+2)+3 or 1+(2+3), not 1+2+3. 
case class Num(value: Int) extends Expr
case class Add(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr
case class Mul(left: Expr, right: Expr) extends Expr
case class Div(left: Expr, right: Expr) extends Expr
case class Negate(expr: Expr) extends Expr
case class Fraction(n: Int, d: Int) extends Expr

// TODO: has too many brackets. Get rid of them algorithmically
def stringify(expr: Expr): String = 
  expr match 
    case Num(n) => if n >= 0 then s"$n" else s"(-$n)"
    case Add(e1, e2) => s"(${stringify(e1)} + ${stringify(e2)})"
    case Sub(e1, e2) => s"(${stringify(e1)} - ${stringify(e2)})"
    case Mul(e1, e2) => s"(${stringify(e1)} * ${stringify(e2)})"
    case Div(e1, e2) => s"(${stringify(e1)} / ${stringify(e2)})"
    case Negate(e) => s"(-${stringify(e)})"

case class Step(desc: String, substeps: List[Step] = List()) {
  def toString(offset: Int = 0): String = 
    " " * offset + desc + "\n" + 
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
    Success(res, List(Step(desc)))

  def substeps[A](sol: Solution[A]): Solution[A] = 
    sol match
      case Success(res, steps) => Success(res, List(Step(desc, steps)))
      case Failure(reason) => Failure(reason)
}

type Rule[A, B] = (A => Solution[B])
def repeat[A](f: Rule[A, A]): Rule[A, A] = cur => {
  for {
    nxt <- f(cur)
    res <- repeat(f)(nxt)
  } yield res
} handleFailureWith (_ => Success(cur, List()))

def tryOrElse(l: List[Solution[A]]): Solution[A] =
  l match 
    case Nil => Failure("Used all lists")
    case hd :: tl => hd handleFailureWith {_ => tryOrElse(tl)}

def simplify(e: Expr): Solution[Expr] = 
  e match 
    case Add(e1, e2) => addNums(e1, e2)
    case _ => Failure("No way to simplify further")

// TODO: handle negative numbers, subtraction, division
def addNums(e1: Expr, e2: Expr): Solution[Expr] = 
  (e1, e2) match 
    case (Num(n1), Num(n2)) => 
      s"Add two integers $n1 and $n2" returns Num(n1 + n2)
      /*
    case (Num(n1), Fraction(a2, d2)) =>
      addNumFraction(e1, e2)
    case (Fraction(a1, d1), Num(n2)) =>
      addFractionNum(e1, e2)
    case (Fraction(a1, d1), Fraction(a2, d2)) =>
      addFractions(e1, e2)
      */
    
    // If nothing works, try reducing some steps
    case (e1, e2) => {
      val first = "Evaluate first term" substeps (for f1 <- simplify(e1) yield Add(f1, e2))
      val second = "Evaluate second term" substeps (for f2 <- simplify(e2) yield Add(e1, f2))
      first.handleFailureWith(_ => second)
    }
    
    /*tryOrElse(List(
      ("Evaluate first term" substeps (for f1 <- simplify(e1) yield Add(f1, e2))),
      ("Evaluate second term" substeps (for f2 <- simplify(e2) yield Add(e1, f2)))
    ))*/

def mulNums(e1: Expr, e2: Expr): Solution[Expr] = 
  (e1, e2) match 
    case (Num(n1), Num(n2)) => 
      s"Multiply two integers $n1 and $n2" returns Num(n1 * n2)

def divNums(e1: Expr, e2: Expr): Solution[Expr] = 
  (e1, e2) match 
    case (Num(n1), Num(n2)) => 
      if n2 == 0
        then Failure("Division by zero!")
      else if n1 % n2 == 0
        then s"Divide two integers $n1 and $n2" returns Num(n1 / n2)
      else 
        s"Make fraction of $n1 and $n2" returns Fraction(n1, n2)
