package foo

import scala.math._

object Drawing {
  def draw(points: Seq[(BigInt, BigInt)]) = {
    val minX = points.map(_._1).min
    val minY = points.map(_._2).min
    val maxX = points.map(_._1).max
    val maxY = points.map(_._2).max
    (minY to maxY).map(y =>
      (minX to maxX).map(x =>
        if (points contains (x, y)) "*" else " "
      ).mkString + "\n"
    ).mkString 
  }

  def multidraw(pics: Seq[Seq[(BigInt, BigInt)]]) =
    pics.map(draw).mkString("vvvv\n", "----\n", "^^^^\n")
}
