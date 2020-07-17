package icfp

import scala.math.BigInt

object Modulate {
  def demodulate(s: String): (Any, String) = (s take 2) match {
    case "00" => (Unit, s drop 2)
    case "11" => {
      val (a, r1) = demodulate(s drop 2)
      val (b, r2) = demodulate(r1)
      ((a, b), r2)
    }
    case "01" => {
      val n = s.indexOf('0') - 2
      val v = BigInt(s.substring(n + 2, n + 2 + n * 4), 2)
      (v, s drop (n + 2 + n * 4))
    }
    case "10" => {
      val n = s.indexOf('0') - 2
      val v = BigInt(s.substring(n + 2, n + 2 + n * 4), 2)
      (-v, s drop (n + 2 + n * 4))
    }
  }

  def modulate(v: Any): String = v match {
    case Unit => "00"
    case (a: Any, b: Any) => "11" + modulate(a) + modulate(b)
    case n: BigInt => if (n > 0) {
      val l = n.bitLength + 3 / 4
      "01" + "1" * (l) + "0" + ("000" + n.toString(2)).takeRight(l * 4)
    } else {
      val l = (-n).bitLength + 3 / 4
      "10" + "1" * (l) + "0" + ("000" + (-n).toString(2)).takeRight(l * 4)
    }
  }
}
