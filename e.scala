trait Expr

// TODO: currently only supports binary operators
// That is, (1+2)+3 or 1+(2+3), not 1+2+3. 
case class Num(value: Int) extends Expr
case class Add(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr
case class Mul(left: Expr, right: Expr) extends Expr
case class Div(left: Expr, right: Expr) extends Expr
case class Negate(expr: Expr) extends Expr

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

case class Solution[A](value: Either[Any, (A, List[Step])]) {
  def map[B](f: A => B): Solution[B] =
    Solution(value.map((a, l) => (f(a), l)))
  def flatMap[B](f: A => Solution[B]): Solution[B] = 
    Solution(value.flatMap((a, asteps) => f(a).value.map((b, bsteps) => (b, asteps ::: bsteps))))
  def withFilter(f: A => Boolean): Solution[A] = 
    value match 
      case Left(err) => Solution(Left(err))
      case Right((a, asteps)) => 
        if f(a) then this else Solution(Left("Test failed")) // TODO
  
  override def toString: String =
    value match 
      case Left(err) => "Fail: " + err.toString
      case Right((a, steps)) => "Success: " + a.toString + "\n" + steps.mkString

  // def entitle(f: A => String): entitles the whole solution from result,
  // making it into one rooted tree
}

implicit def title2solution(desc: String): Solution[Unit] =
  Solution(Right(((), List(Step(desc)))))

implicit class Title(val desc: String) {
  def returns[A](res: A): Solution[A] = 
    Solution(Right((res, List(Step(desc)))))

  def substeps[A](sol: Solution[A]): Solution[A] = 
    Solution(sol.value.map((a, steps) => (a, List(Step(desc, steps)))))
}

def add(n1: Num, n2: Num): Solution[Num] = 
  (n1, n2) match 
    case (Num(n1), Num(n2)) => 
      s"Add two integers $n1 and $n2" returns Num(n1 + n2)

def mul(n1: Num, n2: Num): Solution[Num] = 
  (n1, n2) match 
    case (Num(n1), Num(n2)) => 
      s"Multiply two integers $n1 and $n2" returns Num(n1 * n2)


