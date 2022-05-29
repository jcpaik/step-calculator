### Usage
Type `scala` and enter `:load calc.scala` in console to run the script.
```
> scala
Welcome to Scala 3.1.1 (17.0.2, Java Java HotSpot(TM) 64-Bit Server VM).
Type in expressions for evaluation. Or try :help.

scala> :load calc.scala
(...)
val ex3: String = Input: ((4 + (1 / 5)) * (5 + 5))

Evaluate first term (4 + (1 / 5))  => ((4 + 1/5) * (5 + 5))
  Evaluate second term (1 / 5)  => (4 + 1/5)
    Make fraction of 1 and 5  => 1/5
Evaluate first term (4 + 1/5)  => ((20/5 + 1/5) * (5 + 5))
  Make 4 into a fraction with denominator 5  => 20/5
Evaluate first term (20/5 + 1/5)  => (21/5 * (5 + 5))
  Add numerator of 20/5 and 1/5  => 21/5
Evaluate second term (5 + 5)  => (21/5 * 10)
  Add two integers 5 and 5  => 10
Multiply 10 to numerator of 21/5  => 210/5
  Multiply two integers 21 and 10  => 210

Some(210/5)

scala> 
```