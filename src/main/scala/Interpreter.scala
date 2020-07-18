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
  case class Const[+T](a: T) extends AST[T] {
    def apply() = a
    override def toString() = s"Const($a)"
  }
  case class Func[A,T](n: String, f: (A) => AST[T]) extends AST[(A) => AST[T]] {
    def apply() = (a: A) => {
      println(s"Running $n on $a")
      var r = f(a)
      println(s"Running $n on $a result $r")
      r
    }
    override def toString() = n
  }
  object Func {
    def apply[A,T](n: String, f: (A) => AST[T]): AST[(A) => AST[T]] =
      new Func(n, f)
    def apply[A,B,T](n: String, f: (A, B) => AST[T]): AST[(A) => AST[(B) => AST[T]]] =
      new Func(n, (a: A) => Func(n + "/" + a + "/", (b: B) => f(a, b)))
    def apply[A,B,C,T](n: String, f: (A, B, C) => AST[T]): AST[(A) => AST[(B) => AST[(C) => AST[T]]]] =
      new Func(n, (a: A) => Func(n + "/" + a + "/", (b: B) => Func(n + "/" + a + "/" + b + "/", (c: C) => f(a, b, c))))
  }
  case class Ap(a: AST[Any], b: AST[Any]) extends AST[Any] {
    def apply() = a() match {
      case f: ((_) => Any) => f.asInstanceOf[Fun1](b)
      case Func(n, f) => {
        println(s"Applying $n to $b")
        val r = f.asInstanceOf[Fun1](b)
        println(s"Applying $n to $b result $r")
        r
      }
      case true => Func("t", (x: AST[Any], y: AST[Any]) => x)()(b)
      case false => Func("f", (x: AST[Any], y: AST[Any]) => y)()(b)
      case x => throw new IllegalArgumentException(s"Cannot apply non-function $x")
    }
    override def toString() = s"ap $a $b"
  }
}

object Interpreter {
  def protocol(s: String): AST[Any] = {
    val interpreter = new Interpreter()
    interpreter.runFile(s"$s.txt")
    val p = interpreter.symbols(s)
    println(s"Using protocol $p")
    p
  }
}

class Interpreter {
  val symbols = scala.collection.mutable.Map[String, AST[Any]]()

  import AST._
  case class Lookup(s: String) extends AST[Any] {
    def apply() = symbols(s)
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

  def parse(words: Seq[String]): (AST[Any], Seq[String]) = {
    if (words.head == "ap") {
      val (a, r1) = parse(words.tail)
      val (b, r2) = parse(r1)
      (Ap(a, b), r2)
    } else (words.head match {
      case x if x.forall(_.isDigit) => Const(BigInt(x))
      case x if x(0) == '-' && x.tail.forall(_.isDigit) => Const(BigInt(x))
      case s if s(0) == ':' => Lookup(s)
     
      case "add" => Func("add", (a: AST[BigInt], b: AST[BigInt]) => a() + b())
      case "b" => Func("b", (a: AST[Fun1], b: AST[Fun1], c: AST[Any]) => a()(b()(c)))
      case "c" => Func("c", (a: AST[Fun2], b: AST[Any], c: AST[Any]) => a()(c)(b))
      case "car" => Func("car", (a: AST[Any]) => a() match {
        case l: Seq[_] => Const(l.head)
        case (x, _) => Const(x)
      })
      case "cdr" => Func("cdr", (a: AST[Any]) => a() match {
        case l: Seq[_] => Const(l.tail)
        case (_, x) => Const(x)
      })
      case "cons" => Func("cons", (a: AST[Any], b: AST[Any]) => {
        val (x, y) = (a(), b())
        println(s"Really running cons on $x, $y")
        y match {
          case l: List[_] => x :: l
          case _ => (x, y)
        }
      })
      case "dec" => Func("dec", (a: AST[BigInt]) => a() - 1)
      case "div" => Func("div", (a: AST[BigInt], b: AST[BigInt]) => a() / b())
      case "eq" => Func("eq", (a: AST[Any], b: AST[Any]) => a() == b())
      case "f" => Func("f", (a: AST[Any], b: AST[Any]) => b)
      case "i" => Func("i", (a: AST[Any]) => a)
      case "if0" => Func("if0", (a: AST[BigInt], b: AST[Any], c: AST[Any]) => if (a() == 0) b else c)
      case "inc" => Func("inc", (a: AST[BigInt]) => a() + 1)
      case "isnil" => Func("isnil", (a: AST[Any]) => a() == Nil)
      case "lt" => Func("lt", (a: AST[BigInt], b: AST[BigInt]) => a() < b())
      case "mul" => Func("mul", (a: AST[BigInt], b: AST[BigInt]) => a() * b())
      case "neg" => Func("neg", (a: AST[BigInt]) => -(a()))
      case "nil" => Const(Nil)
      case "pwr2" => Func("pwr2", (a: AST[BigInt]) => BigInt(1) << a().toInt)
      case "s" => Func("s", (a: AST[Fun2], b: AST[Fun1], c: AST[Any]) => a()(c)(b()(c)))
      case "t" => Func("t", (a: AST[Any], b: AST[Any]) => a)

      case _ => throw new IllegalArgumentException(s"Unknown operator ${words.head}")
    }, words.tail)
  }
}
