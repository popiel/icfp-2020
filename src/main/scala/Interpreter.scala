import scala.io._
import scala.math.BigInt

sealed trait AST extends Function0[Any]
object AST {
  case class Const(a: Any) extends AST { def apply() = a }
  case class Func(f: (Any) => Any) extends AST { def apply() = f }
  case class Ap(a: AST, b: AST) extends AST {
    def apply() = a() match {
      case f: ((Any) => Any) => f(b())
      case _ => throw new IllegalArgumentException(s"Cannot apply non-function $a to $b")
    }
  }
  case object Cons extends AST {
    def apply() = { (a: Any) => { (b: Any) => b match {
      case l: List[_] => a :: l
      case _ => (a, b)
    } } }
  }
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
      case "cons" => (Cons, words.tail)
      case "nil" => (Const(Nil), words.tail)

      case _ => throw new IllegalArgumentException(s"Unknown operator ${words.head}")
    }
  }

  def ap(a: Any, b: Any) = a match {
    case f: ((Any) => Any) => {
      println(s"About to apply $f to $b")
      val r = f(b)
      println(s"Yielded $r")
      r
    }
    case _ => throw new IllegalArgumentException(s"cannot apply non-function $a to $b")
  }

  def cons(a: Any, b: Any) = b match {
    case l: List[_] => a :: l
    case _ => (a, b)
  }
}
