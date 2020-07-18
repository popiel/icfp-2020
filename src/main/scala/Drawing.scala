package foo

import scala.math._

object Drawing {
  def draw(points: Seq[(BigInt, BigInt)]) {
    val minX = points.map(_._1).min
    val minY = points.map(_._2).min
    val maxX = points.map(_._1).max
    val maxY = points.map(_._2).max
    for (y <- minY to maxY) {
      for (x <- minX to maxX) {
        print(if (points contains (x, y)) "*" else " ")
      }
      println()
    }
  }

  def multidraw(pics: Seq[Seq[(BigInt, BigInt)]]) {
    println("vvvv")
    if (pics.nonEmpty) {
      draw(pics.head)
      for(pic <- pics.tail) {
        println("----")
        draw(pic)
      }
    }
    println("^^^^")
  }
}
