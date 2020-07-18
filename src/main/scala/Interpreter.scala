package foo

import scala.io._
import scala.math.BigInt

sealed trait AST extends Function0[Any]
object AST {
  type Fun1 = (Any) => Any
  type Fun2 = (Any) => Fun1
  case class Func[A,T](n: String, f: (A) => T) extends AST {
    def apply() = (a: A) => {
      println(s"Running $n on $a")
      var r = f(a)
      println(s"Running $n on $a result $r")
      r
    }
    override def toString() = n
  }
  object Func {
    def apply[A,T](n: String, f: (A) => T): AST =
      new Func(n, f)
    def apply[A,B,T](n: String, f: (A, B) => T): AST =
      new Func(n, (a: A) => ((b: B) => f(a, b)))
    def apply[A,B,C,T](n: String, f: (A, B, C) => T): AST =
      new Func(n, (a: A) => ((b: B) => ((c: C) => f(a, b, c))))
  }
  case class Ap(a: Any, b: Any) extends AST {
    def apply() = a match {
      case Func(n, f) => {
        println(s"Applying $n to $b")
        val r = f.asInstanceOf[Fun1](b)
        println(s"Applying $n to $b result $r")
        r
      }
      case true => Ap(Func("t", (x: Any, y: Any) => x), b)()
      case false => Ap(Func("f", (x: Any, y: Any) => y), b)()
      case x: AST => Ap(x(), b)()
      case f: ((_) => Any) => f.asInstanceOf[Fun1](b)
      case x => throw new IllegalArgumentException(s"Cannot apply non-function $x")
    }
    override def toString() = s"ap $a $b"
  }

  def extract[T](a: Any)(implicit m: Manifest[T]): T = a match {
    case x: AST => extract[T](x())
    case m(x) => x
    case _ => throw new IllegalArgumentException(s"Cannot extract a $m from $a")
  }
}

object Interpreter {
  def protocol(s: String): Any = {
    val interpreter = new Interpreter()
    interpreter.runFile(s"$s.txt")
    val p = interpreter.symbols(s)
    println(s"Using protocol $p")
    p
  }
}

class Interpreter {
  val symbols = scala.collection.mutable.Map[String, Any]()

  import AST._
  case class Lookup(s: String) extends AST {
    def apply() = {
      println(s"lookup $s yields ${symbols(s)}")
      symbols(s)
    }
    override def toString() = s
  }

  def runFile(s: String) {
    Source.fromFile(s).getLines.foreach(run)
  }

  def run(s: String) {
    val words = s.split(" ")
    if (words.length > 2 && words(1) == "=") {
      if (symbols contains words(0)) {
        println(s"WARNING: Redefinition of ${words(0)}")
      }
      val (parsed, extra) = parse(words drop 2)
      if (extra.nonEmpty) {
        println(s"Extra junk at end of definition of ${words(0)}: $extra")
      }
      symbols(words(0)) = parsed
      println(s"Defined ${words(0)} as $parsed")
    } else {
      println(s"Non-definition line $s")
    }
  }

  def parse(words: Seq[String]): (Any, Seq[String]) = {
    if (words.head == "ap") {
      val (a, r1) = parse(words.tail)
      val (b, r2) = parse(r1)
      (Ap(a, b), r2)
    } else (words.head match {
      case x if x.forall(_.isDigit) => BigInt(x)
      case x if x(0) == '-' && x.tail.forall(_.isDigit) => BigInt(x)
      case s if s(0) == ':' => Lookup(s)
     
      case "add" => Func("add", (a: Any, b: Any) => extract[BigInt](a) + extract[BigInt](b))
      case "b" => Func("b", (a: Any, b: Any, c: Any) => {
        Ap(a, Ap(b, c))()
      })
      case "c" => Func("c", (a: Any, b: Any, c: Any) => {
        Ap(Ap(a, c), b)()
      })
      case "car" => Func("car", (a: Any) => extract[Any](a) match{
        case l: Seq[_] => l.head
        case (x, _) => x
      })
      case "cdr" => Func("cdr", (a: Any) => extract[Any](a) match {
        case l: Seq[_] => l.tail
        case (_, x) => x
      })
      case "cons" => Func("cons", (a: Any, b: Any) => {
        val (x, y) = (extract[Any](a), extract[Any](b))
        println(s"Really running cons on $x, $y")
        y match {
          case l: List[_] => x :: l
          case _ => (x, y)
        }
      })
      case "dec" => Func("dec", (a: Any) => extract[BigInt](a) - 1)
      case "div" => Func("div", (a: Any, b: Any) => extract[BigInt](a) / extract[BigInt](b))
      case "eq" => Func("eq", (a: Any, b: Any) => extract[Any](a) == extract[Any](b))
      case "f" => Func("f", (a: Any, b: Any) => b)
      case "i" => Func("i", (a: Any) => a)
      case "if0" => Func("if0", (a: Any, b: Any, c: Any) => if (extract[BigInt](a) == 0) b else c)
      case "inc" => Func("inc", (a: Any) => extract[BigInt](a) + 1)
      case "isnil" => Func("isnil", (a: Any) => extract[Any](a) == Nil)
      case "lt" => Func("lt", (a: Any, b: Any) => extract[BigInt](a) < extract[BigInt](b))
      case "mul" => Func("mul", (a: Any, b: Any) => extract[BigInt](a) * extract[BigInt](b))
      case "neg" => Func("neg", (a: Any) => -(extract[BigInt](a)))
      case "nil" => Nil
      case "pwr2" => Func("pwr2", (a: Any) => BigInt(1) << extract[BigInt](a).toInt)
      case "s" => Func("s", (a: Any, b: Any, c: Any) =>
        Ap(Ap(a,c),Ap(b,c))()
      )
      case "t" => Func("t", (a: Any, b: Any) => a)

      case _ => throw new IllegalArgumentException(s"Unknown operator ${words.head}")
    }, words.tail)
  }
}
