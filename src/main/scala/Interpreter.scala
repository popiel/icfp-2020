import scala.io._
import scala.math.BigInt

sealed trait AST extends Function0[Any]
object AST {
  type Fun1 = (Any) => Any
  type Fun2 = (Any) => ((Any) => Any)
  case class Const(a: Any) extends AST { def apply() = a }
  case class Func[A](f: (A) => Any) extends AST { def apply() = f }
  object Func {
    def apply[A](f: (A) => Any) = new Func(f)
    def apply[A,B](f: (A, B) => Any) =
      new Func(((a: A) => ((b: B) => f(a, b))))
    def apply[A,B,C](f: (A, B, C) => Any) =
      new Func(((a: A) => ((b: B) => ((c: C) => f(a, b, c)))))
  }
  case class Ap(a: AST, b: AST) extends AST {
    def apply() = a() match {
      case f: ((_) => Any) => f.asInstanceOf[(Any) => Any](b())
      case true => Func((x: Any, y: Any) => x)()(b())
      case false => Func((x: Any, y: Any) => y)()(b())
      case _ => throw new IllegalArgumentException(s"Cannot apply non-function $a to $b")
    }
  }
  val Cons = Func((a: Any, b: Any) => b match {
    case l: List[_] => a :: l
    case _ => (a, b)
    }
  )
}

class Interpreter {
  val symbols = scala.collection.mutable.Map[String, Any]()

  import AST._
  case class Lookup(s: String) extends AST { def apply() = symbols(s) }

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
      println(s"Defined ${words(0)}")
    } else {
      println(s"Non-definition line $s")
    }
  }

  def parse(words: Seq[String]): (AST, Seq[String]) = {
    if (words.head == "ap") {
      val (a, r1) = parse(words.tail)
      val (b, r2) = parse(r1)
      (Ap(a, b), r2)
    } else (words.head match {
      case x if x.forall(_.isDigit) => Const(BigInt(x))
      case x if x(0) == '-' && x.tail.forall(_.isDigit) => Const(BigInt(x))
      case s if s(0) == ':' => Lookup(s)
     
      case "add" => Func((a: BigInt, b: BigInt) => a + b)
      case "b" => Func((a: Fun1, b: Fun1, c: Any) => a(b(c)))
      case "c" => Func((a: Fun2, b: Any, c: Any) => a(c)(b))
      case "car" => Func((a: Any) => a match {
        case l: List[_] => l.head
        case (x, _) => x
      })
      case "cdr" => Func((a: Any) => a match {
        case l: List[_] => l.tail
        case (_, x) => x
      })
      case "cons" => Cons
      case "dec" => Func((a: BigInt) => a - 1)
      case "div" => Func((a: BigInt, b: BigInt) => a / b)
      case "eq" => Func((a: Any, b: Any) => a == b)
      case "f" => Func((a: Any, b: Any) => b)
      case "i" => Func((a: Any) => a)
      case "inc" => Func((a: BigInt) => a + 1)
      case "isnil" => Func((a: Any) => a == Nil)
      case "lt" => Func((a: BigInt, b: BigInt) => a < b)
      case "mul" => Func((a: BigInt, b: BigInt) => a * b)
      case "neg" => Func((a: BigInt) => -a)
      case "nil" => Const(Nil)
      case "pwr2" => Func((a: BigInt) => BigInt(1) << a.toInt)
      case "s" => Func((a: Fun2, b: Fun1, c: Any) => a(c)(b(c)))
      case "t" => Func((a: Any, b: Any) => a)

      case _ => throw new IllegalArgumentException(s"Unknown operator ${words.head}")
    }, words.tail)
  }
}
