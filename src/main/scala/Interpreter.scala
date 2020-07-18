package foo

import scala.io._
import scala.math.BigInt

sealed trait AST[+T] extends Function0[T]
object AST {
  implicit def bigint2AST(b: BigInt) = Const(b)
  implicit def boolean2AST(b: Boolean) = Const(b)
  implicit def pair2AST(p: (_,_)) = Const(p)
  implicit def seq2AST(l: Seq[_]) = Const(l)

  type Fun1 = (AST[Any]) => AST[Any]
  type Fun2 = (AST[Any]) => Fun1
  case class Const[+T](a: T) extends AST[T] { def apply() = a }
  case class Func[A,T](f: (A) => AST[T]) extends AST[(A) => AST[T]] { def apply() = f }
  object Func {
    def apply[A,T](f: (A) => AST[T]): AST[(A) => AST[T]] = new Func(f)
    def apply[A,B,T](f: (A, B) => AST[T]): AST[(A) => AST[(B) => AST[T]]] =
      new Func((a: A) => Func((b: B) => f(a, b)))
    def apply[A,B,C,T](f: (A, B, C) => AST[T]): AST[(A) => AST[(B) => AST[(C) => AST[T]]]] =
      new Func((a: A) => Func((b: B) => Func((c: C) => f(a, b, c))))
  }
  case class Ap(a: AST[Any], b: AST[Any]) extends AST[Any] {
    def apply() = a() match {
      case f: ((_) => Any) => f.asInstanceOf[Fun1](b)
      case true => Func((x: AST[Any], y: AST[Any]) => x)()(b)
      case false => Func((x: AST[Any], y: AST[Any]) => y)()(b)
      case _ => throw new IllegalArgumentException(s"Cannot apply non-function ${a()}")
    }
  }
}

object Interpreter {
  def protocol(s: String): AST.Fun2 = {
    val p = new Interpreter()
    p.runFile(s"$s.txt")
    p.symbols(s)().asInstanceOf[AST.Fun2]
  }
}

class Interpreter {
  val symbols = scala.collection.mutable.Map[String, AST[Any]]()

  import AST._
  case class Lookup(s: String) extends AST[Any] { def apply() = symbols(s) }

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

  def parse(words: Seq[String]): (AST[Any], Seq[String]) = {
    if (words.head == "ap") {
      val (a, r1) = parse(words.tail)
      val (b, r2) = parse(r1)
      (Ap(a, b), r2)
    } else (words.head match {
      case x if x.forall(_.isDigit) => Const(BigInt(x))
      case x if x(0) == '-' && x.tail.forall(_.isDigit) => Const(BigInt(x))
      case s if s(0) == ':' => Lookup(s)
     
      case "add" => Func((a: AST[BigInt], b: AST[BigInt]) => a() + b())
      case "b" => Func((a: AST[Fun1], b: AST[Fun1], c: AST[Any]) => a()(b()(c)))
      case "c" => Func((a: AST[Fun2], b: AST[Any], c: AST[Any]) => a()(c)(b))
      case "car" => Func((a: AST[Any]) => a() match {
        case l: Seq[_] => Const(l.head)
        case (x, _) => Const(x)
      })
      case "cdr" => Func((a: AST[Any]) => a() match {
        case l: Seq[_] => Const(l.tail)
        case (_, x) => Const(x)
      })
      case "cons" => Func((a: AST[Any], b: AST[Any]) => b() match {
        case l: List[_] => a() :: l
        case x => (a(), x)
      })
      case "dec" => Func((a: AST[BigInt]) => a() - 1)
      case "div" => Func((a: AST[BigInt], b: AST[BigInt]) => a() / b())
      case "eq" => Func((a: AST[Any], b: AST[Any]) => a() == b())
      case "f" => Func((a: AST[Any], b: AST[Any]) => b)
      case "i" => Func((a: AST[Any]) => a)
      case "if0" => Func((a: AST[BigInt], b: AST[Any], c: AST[Any]) => if (a() == 0) b else c)
      case "inc" => Func((a: AST[BigInt]) => a() + 1)
      case "isnil" => Func((a: AST[Any]) => a() == Nil)
      case "lt" => Func((a: AST[BigInt], b: AST[BigInt]) => a() < b())
      case "mul" => Func((a: AST[BigInt], b: AST[BigInt]) => a() * b())
      case "neg" => Func((a: AST[BigInt]) => -(a()))
      case "nil" => Const(Nil)
      case "pwr2" => Func((a: AST[BigInt]) => BigInt(1) << a().toInt)
      case "s" => Func((a: AST[Fun2], b: AST[Fun1], c: AST[Any]) => a()(c)(b()(c)))
      case "t" => Func((a: AST[Any], b: AST[Any]) => a)

      case _ => throw new IllegalArgumentException(s"Unknown operator ${words.head}")
    }, words.tail)
  }
}
