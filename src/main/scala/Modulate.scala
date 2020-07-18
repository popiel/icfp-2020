package foo

import scala.math.BigInt

object Modulate {
  def demodulate(s: String): (Any, String) = (s take 2) match {
    case "00" => (Nil, s drop 2)
    case "11" => {
      val (a, r1) = demodulate(s drop 2)
      val (b, r2) = demodulate(r1)
      b match {
        case l: List[Any] => (a :: l, r2)
        case _ => ((a, b), r2)
      }
    }
    case "01" => {
      val n = s.indexOf('0', 2) - 2
      val v = if (n > 0) BigInt(s.substring(n + 3, n + 3 + n * 4), 2) else BigInt(0)
      (v, s.drop(n + 3 + n * 4))
    }
    case "10" => {
      val n = s.indexOf('0', 2) - 2
      val v = if (n > 0) BigInt(s.substring(n + 3, n + 3 + n * 4), 2) else BigInt(0)
      (-v, s.drop(n + 3 + n * 4))
    }
  }

  def modulate(v: Any): String = v match {
    case Nil => "00"
    case (a: Any, b: Any) => "11" + modulate(a) + modulate(b)
    case a :: b => "11" + modulate(a) + modulate(b)
    case n: Int => modulate(BigInt(n))
    case n: Long => modulate(BigInt(n))
    case n: BigInt => if (n >= 0) {
      val l = (n.bitLength + 3) / 4
      "01" + "1" * (l) + "0" + ("000" + n.toString(2)).takeRight(l * 4)
    } else {
      val l = ((-n).bitLength + 3) / 4
      "10" + "1" * (l) + "0" + ("000" + (-n).toString(2)).takeRight(l * 4)
    }
  }
}
