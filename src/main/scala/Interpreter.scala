package foo

import scala.io._
import scala.math.BigInt

sealed trait AST extends Function0[Any]
trait Complete extends AST
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
      new Func(n, (a: A) =>
      new Func(s"$n(_,?)", (b: B) => f(a, b)))
    def apply[A,B,C,T](n: String, f: (A, B, C) => T): AST =
      new Func(n, (a: A) =>
      new Func(s"$n(_,?,?)", (b: B) =>
      new Func(s"$n(_,_,?)", (c: C) => f(a, b, c))))
  }
  case class Ap(a: Any, b: Any) extends Complete {
    var result: Option[Any] = None
    def compute() {
      result = Some(a match {
        case Func(n, f) => {
          val s = n.toString
          // println(s"Running $s")
          // println(s"Applying $b to $s")
          val r = f.asInstanceOf[Fun1](b)
          // println(s"Yielded $r from $s($b)")
          r
        }
        case true => Ap(Func("t", (x: Any, y: Any) => x), b)()
        case false => Ap(Func("f", (x: Any, y: Any) => y), b)()
	case x :: y => Ap(Ap(b, x), y)()
	case (x, y) => Ap(Ap(b, x), y)()
        case x: Complete => Ap(x(), b)()
        case x => throw new IllegalArgumentException(s"Cannot apply non-function $x")
      })
    }
    def apply() = result.getOrElse { compute(); result.get }
    override def toString() = result.map(_.toString).getOrElse(s"ap $a $b")
  }

  def extract[T](a: Any)(implicit m: Manifest[T]): T = a match {
    case x: Complete =>
      val s = x.toString
      // println(s"Running $m from $s")
      val r = extract[T](x())
      // println(s"yielded $r from $s")
      r
    case m(x) => x
    case _ => throw new IllegalArgumentException(s"Cannot extract $m from $a")
  }
}

object Interpreter {
  def protocol(s: String): Any = {
    val interpreter = new Interpreter()
    interpreter.runFile(s"$s.txt")
    val p = interpreter.symbols(s)
    // println(s"Using protocol $p")
    p
  }
}

class Interpreter {
  val symbols = scala.collection.mutable.Map[String, Any]()

  import AST._
  case class Lookup(s: String) extends Complete {
    def apply() = {
      // println(s"lookup $s yields ${symbols(s)}")
      // println(s"lookup $s")
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

  def parse(vars: Map[String, Any], line: String): Any = {
    val (r, extra) = parse(vars, line.split(" "))
    if (extra.nonEmpty) {
      println(s"Extra junk at end of special definition: $extra")
    }
    r
  }

  def parse(words: Seq[String]): (Any, Seq[String]) =
    parse(Map(), words)

  def parse(vars: Map[String, Any], words: Seq[String]): (Any, Seq[String]) = {
    if (words.head == "ap") {
      val (a, r1) = parse(vars, words.tail)
      val (b, r2) = parse(vars, r1)
      (Ap(a, b), r2)
    } else (words.head match {
      case x if vars contains x => vars(x)

      case x if x.forall(_.isDigit) => BigInt(x)
      case x if x(0) == '-' && x.tail.forall(_.isDigit) => BigInt(x)
      case s if s(0) == ':' => Lookup(s)
     
      case "add" => Func("add", (a: Any, b: Any) =>
        extract[BigInt](a) + extract[BigInt](b)
      )
      case "b" => Func("b", (a: Any, b: Any, c: Any) =>
        Ap(a, Ap(b, c))()
      )
      case "c" => Func("c", (a: Any, b: Any, c: Any) =>
        Ap(Ap(a, c), b)()
      )
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
        // println(s"Really running cons on $x, $y")
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

      case "modem" => Func("modem", (a: Any) => { IO.store(a); a })
      case "draw" => Func("draw", (a: Any) => 
        println(Drawing.draw(extract[Seq[(BigInt, BigInt)]](a)))
      )
      case "multidraw" => Func("multidraw", (a: Any) =>
        println(Drawing.multidraw(extract[Seq[Seq[(BigInt, BigInt)]]](a)))
      )
      case "send" => Func("send", (a: Any) => IO.send(a))

      case "f38" => Func("f38", (x2: Any, x0: Any) =>
        extract[Any](parse(
          Map("x2" -> x2, "x0" -> x0),
          "ap ap ap if0 ap car x0 ap ap cons ap modem ap car ap cdr x0 ap ap cons ap multidraw ap car ap cdr ap cdr x0 nil ap ap ap interact x2 ap modem ap car ap cdr x0 ap send ap car ap cdr ap cdr x0"
        ))
      )
      case "interact" => Func("interact", (x2: Any, x4: Any, x3: Any) =>
        extract[Any](parse(
          Map("x2" -> x2, "x4" -> x4, "x3" -> x3),
          "ap ap f38 x2 ap ap x2 x4 x3"
        ))
      )

      case _ => throw new IllegalArgumentException(s"Unknown operator ${words.head}")
    }, words.tail)
  }
}
