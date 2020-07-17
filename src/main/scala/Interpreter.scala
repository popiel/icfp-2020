import scala.io._
import scala.math.BigInt

sealed trait AST extends Function0[Any]
object AST {
  type Fun1 = (Any) => Any
  type Fun2 = (Any) => ((Any) => Any)
  case class Const(a: Any) extends AST { def apply() = a }
  case class Func(f: Fun1) extends AST { def apply() = f }
  object Func {
    def apply(f: Fun1) = new Func(f)
    def apply(f: (Any, Any) => Any) =
      new Func((a: Any) => ((b: Any) => f(a, b)))
    def apply[A,B,C](f: (A, B, C) => Any) =
      new Func(((a: A) => ((b: B) => ((c: C) => f(a, b, c)))).asInstanceOf[Fun1])
  }
  case class Ap(a: AST, b: AST) extends AST {
    def apply() = a() match {
      case f: ((Any) => Any) => f(b())
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
    words.head match {
      case x if x.forall(_.isDigit) => (Const(BigInt(x)), words.tail)
      case s if s(0) == ':' => (Lookup(s), words.tail)

      case "ap" => {
        val (a, r1) = parse(words.tail)
        val (b, r2) = parse(r1)
        (Ap(a, b), r2)
      }
      case "c" => (Func((a: Fun2, b: Any, c: Any) => a(c)(b)), words.tail)
      case "cons" => (Cons, words.tail)
      case "neg" => (Func(_ match {
        case n: BigInt => -n
        case x => throw new IllegalArgumentException(s"Cannot negate non-integer $x")
      }), words.tail)
      case "nil" => (Const(Nil), words.tail)
      case "s" => (Func((a: Fun2, b: Fun1, c: Any) => a(c)(b(c))), words.tail)

      case _ => throw new IllegalArgumentException(s"Unknown operator ${words.head}")
    }
  }
}
