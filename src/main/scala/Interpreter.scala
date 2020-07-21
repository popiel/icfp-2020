package foo

import scala.annotation._
import scala.io._
import scala.math.BigInt

sealed trait AST extends Function0[Any]
trait Complete extends AST
trait Lookup extends Complete
object AST {
  var parseCount = 0
  var apCount = 0
  var compCount = 0

  type Fun1 = (Any) => Any
  case class Func1(n: String, f: Fun1) extends AST {
    def apply() = f
    override def toString() = n
  }
  case class Func2(n: String, f: (Any, Any) => Any) extends AST {
    def apply() = f
    override def toString() = n
  }
  case class Func3(n: String, f: (Any, Any, Any) => Any) extends AST {
    def apply() = f
    override def toString() = n
  }
  object Func {
    def apply(n: String, f: Fun1): AST =
      new Func1(n, f)
    def apply(n: String, f: (Any, Any) => Any): AST =
      new Func2(n, f)
    def apply(n: String, f: (Any, Any, Any) => Any): AST =
      new Func3(n, f)
  }

  @tailrec
  def flip(a: Any, acc: List[Ap] = Nil): (Any, List[Ap]) = {
    a match {
      case l: Lookup => flip(l(), acc)
      case ap: Ap if ap.result.nonEmpty => flip(ap.result.get, acc)
      case ap: Ap => flip(ap.a, ap :: acc)
      case x => (x, acc)
    }
  }

  def flop(ins: (Any, List[Ap])): Any =
    ins._2.foldLeft(ins._1){ (acc, x) => Ap(acc, x.b) }

  @tailrec
  def crunch(instructions: (Any, List[Ap])): (Any, List[Ap]) = {
    // println("crunching: " + instructions)
    instructions match {
      case (f: Func1, x :: r) =>
        val o = f.f(x.b)
	x.result = Some(o)
        crunch(flip(o, r))
      case (f: Func2, x :: y :: r) =>
        val o = f.f(x.b, y.b)
	y.result = Some(o)
        crunch(flip(o, r))
      case (f: Func3, x :: y :: z :: r) =>
        val o = f.f(x.b, y.b, z.b)
	z.result = Some(o)
        crunch(flip(o, r))
      case (true, x :: y :: r) =>
        y.result = Some(x.b)
        crunch(flip(x.b, r))
      case (false, x :: y :: r) =>
        y.result = Some(y.b)
        crunch(flip(y.b, r))
      case (Nil, x :: r) =>
        x.result = Some(true)
        crunch((true, r))
      case ((x, y), b :: r) =>
        b.result = Some(Ap(Ap(b.b, x), y))
        crunch(flip(b.result.get, r))
      case _ => instructions
    }
  }

  case class Ap(a: Any, b: Any) extends Complete {
    var result: Option[Any] = None

    def compute(): Any = {
      compCount += 1
      val i = flip(this)
      val o = crunch(i)
      if (o eq i) this else flop(o)
    }
    def apply() = {
      // do this instead of lazy val because it's faster (no locks)
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

  @tailrec def mapList(l : Any, acc: (Any) => Any, f: (Any) => Any): Any = {
    l match {
      case (h, t) => mapList(t, (o) => acc((f(h), o)), f)
      case _ => acc(f(l))
    }
  }

  def strict(a: Any): Any = a match {
    case x: Complete => strict(x())
    case (x, y) => mapList(a, (z) => z, strict(_))
    case x => x
  }

  def strictNoFollow(a: Any): Any = a match {
    case x: Ap => strictNoFollow(x())
    case (x, y) => mapList(a, (z) => z, strictNoFollow(_))
    case x => x
  }

  @tailrec final def listify(a: Any, acc: (Any) => Any = (x) => x): Any = {
    extract[Any](a) match {
      case (h, t) => listify(t, (o) => acc(o match {
        case l: List[_] => h :: l
        case _ => (h, o)
      }))
      case _ => acc(a)
    }
  }


  @tailrec
  def interact(protocol: Any, state: Any, vector: Any): Any = {
    apCount = 0
    compCount = 0
    parseCount = 0

    val (flag, x1) = extract[(Any, Any)](Ap(Ap(protocol, state), vector))
    val (newState, x2) = extract[(Any, Any)](x1)
    val (data, _) = extract[(Any, Any)](x2)

    println(s"Running: apCount: $apCount  compCount: $compCount  parseCount: $parseCount")
    apCount = 0
    compCount = 0
    parseCount = 0
    val strictNewState = IO.store(newState)
    println(s"Storing: apCount: $apCount  compCount: $compCount  parseCount: $parseCount")
    if (flag == 0) {
      apCount = 0
      compCount = 0
      parseCount = 0
      Drawing.multidraw(data)
      println(s"Drawing: apCount: $apCount  compCount: $compCount  parseCount: $parseCount")
      strictNewState
    } else {
      interact(protocol, strictNewState, IO.send(strict(data)))
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
  val condensed = scala.collection.mutable.Map[String, Any]()

  symbols("interact") = Func("interact", interact _)

  case class Labeled(s: String) extends Lookup {
    lazy val apply = {
      condensed.getOrElseUpdate(s, symbols(s) match {
        case a: Ap => 
          // println("Condensing symbol " + s + " from " + a)
          val o = flop(crunch(flip(symbols(s))))
          // println("Condensed symbol " + s + " to " + o)
	  o
	case x => x
      })
    }
    override def toString() = s
  }

  def runFile(s: String) {
    Source.fromFile(s).getLines.foreach(run)
    // for (k <- symbols.keys) symbols(k) = extract[Any](symbols(k))
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

  def parse(words: Seq[String]): (Any, Seq[String]) = {
    parseCount += 1
    if (words.head == "ap") {
      val (a, r1) = parse(words.tail)
      val (b, r2) = parse(r1)
      (Ap(a, b), r2)
    } else (words.head match {
      case x if x.forall(_.isDigit) => BigInt(x)
      case x if x(0) == '-' && x.tail.forall(_.isDigit) => BigInt(x)

      case s if s(0) == ':' => Labeled(s)
     
      case "add" => Func("add", (a: Any, b: Any) =>
        extract[BigInt](a) + extract[BigInt](b)
      )
      case "b" => Func("b", (a: Any, b: Any, c: Any) =>
        Ap(a, Ap(b, c))
      )
      case "c" => Func("c", (a: Any, b: Any, c: Any) =>
        Ap(Ap(a, c), b)
      )
      case "car" => Func("car", (a: Any) => extract[Any](a) match{
        case l: Seq[_] => l.head
        case (x, _) => x
      })
      case "cdr" => Func("cdr", (a: Any) => extract[Any](a) match {
        case l: Seq[_] => l.tail
        case (_, x) => x
      })
      case "cons" => Func("cons", (a: Any, b: Any) => (a, b))
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
        Ap(Ap(a,c),Ap(b,c))
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

      case _ => throw new IllegalArgumentException(s"Unknown operator ${words.head}")
    }, words.tail)
  }

}
