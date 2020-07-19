package foo

import scala.annotation._
import scala.io._
import scala.math.BigInt

sealed trait AST extends Function0[Any]
trait Complete extends AST
object AST {
  var parseCount = 0
  var apCount = 0
  var compCount = 0

  type Fun1 = (Any) => Any
  case class Func(n: String, f: Fun1) extends AST {
    def apply() = f
    override def toString() = n
  }
  object Func {
    def apply(n: String, f: Fun1): AST =
      new Func(n, f)
    def apply(n: String, f: (Any, Any) => Any): AST =
      new Func(n, (a: Any) =>
      new Func(s"$n(_,?)", (b: Any) => f(a, b)))
    def apply(n: String, f: (Any, Any, Any) => Any): AST =
      new Func(n, (a: Any) =>
      new Func(s"$n(_,?,?)", (b: Any) =>
      new Func(s"$n(_,_,?)", (c: Any) => f(a, b, c))))
  }
  case class Ap(a: Any, b: Any) extends Complete {
    var result: Option[Any] = None
    def compute(): Any = { compCount += 1; a match {
      case Func(n, f) => {
        val s = n.toString
        // println(s"Running $s")
        // println(s"Applying $b to $s")
        val r = f(b)
        // println(s"Yielded $r from $s($b)")
        r
      }
      case true => Ap(Func("t", (x: Any, y: Any) => x), b)
      case false => Ap(Func("f", (x: Any, y: Any) => y), b)
      case x :: y => Ap(Ap(b, x), y)
      case (x, y) => Ap(Ap(b, x), y)
      case Nil => true
      case x: Complete => Ap(x(), b)
      case x => throw new IllegalArgumentException(s"Cannot apply non-function $x")
    } }
    def apply() = {
      apCount += 1
      if (result.isEmpty) result = Some(compute())
      result.get
    }
    override def toString() = result.map(_.toString).getOrElse(s"ap $a $b")
  }

  @tailrec def extract[T](a: Any)(implicit m: Manifest[T]): T = a match {
    case x: Complete => extract[T](x())
    case m(x) => x
    case _ => throw new IllegalArgumentException(s"Cannot extract $m from $a")
  }
  def strict(a: Any): Any = a match {
    case x: Complete => strict(x())
    case (x, y) => (strict(x), strict(y))
    case x: List[_] => x.map(strict(_))
    case x => x
  }

  @tailrec
  def interact(protocol: Any, state: Any, vector: Any): List[Any] = {
    println(s"apCount: $apCount  compCount: $compCount  parseCount: $parseCount")
    apCount = 0
    compCount = 0
    parseCount = 0

    val flag :: newState :: data :: _ =
      extract[List[Any]](Ap(Ap(protocol, state), vector))
    val strictNewState = IO.store(newState)
    if (flag == 0) {
      List(strictNewState, Drawing.multidraw(extract[Seq[Seq[(BigInt, BigInt)]]](data)))
    } else {
      interact(protocol, strictNewState, IO.send(data))
    }
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
  import AST._

  val symbols = scala.collection.mutable.Map[String, Any]()

  symbols("interact") = Func("interact", interact _)

  case class Lookup(s: String) extends Complete {
    lazy val apply = symbols(s)
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
      if (words(0)(0) != ':') println(s"Defined ${words(0)} as $parsed")
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
    parseCount += 1
    if (words.head == "ap") {
      val (a, r1) = parse(vars, words.tail)
      val (b, r2) = parse(vars, r1)
      (Ap(a, b), r2)
    } else (words.head match {
      case x if vars contains x => vars(x)

      case x if x.forall(_.isDigit) => BigInt(x)
      case x if x(0) == '-' && x.tail.forall(_.isDigit) => BigInt(x)
     
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

      case "modem" => Func("modem", (a: Any) => IO.store(a))
      case "draw" => Func("draw", (a: Any) => 
        println(Drawing.draw(extract[Seq[(BigInt, BigInt)]](a)))
      )
      case "multidraw" => Func("multidraw", (a: Any) =>
        println(Drawing.multidraw(extract[Seq[Seq[(BigInt, BigInt)]]](a)))
      )
      case "send" => Func("send", (a: Any) => IO.send(a))

      case s => Lookup(s)

      case _ => throw new IllegalArgumentException(s"Unknown operator ${words.head}")
    }, words.tail)
  }

}
