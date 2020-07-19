package foo

import scala.concurrent._
import scala.math._

import javax.swing._
import java.awt._
import java.awt.event._

object Drawing {
  def draw(points: Seq[(BigInt, BigInt)]) = {
    if (points.isEmpty) ""
    else {
      val minX = points.map(_._1).min
      val minY = points.map(_._2).min
      val maxX = points.map(_._1).max
      val maxY = points.map(_._2).max
      s"($minX,$minY) -> ($maxX,$maxY)\n" + 
      (minY to maxY).map(y =>
        (minX to maxX).map(x =>
          if (points contains (x, y)) "*" else " "
        ).mkString + "\n"
      ).mkString 
    }
  }

  val initialSize = (1000, 800)

  var thePics: Seq[Seq[(BigInt, BigInt)]] = Nil
  lazy val frame: Frame = {
    val f = new Frame("Welcome to the Galaxy!")
    f.addWindowListener(new WindowListener() {
      def windowActivated(x$1: java.awt.event.WindowEvent) {}
      def windowClosed(e: WindowEvent) { System.exit(0) }
      def windowClosing(x$1: java.awt.event.WindowEvent) { f.dispose() }
      def windowDeactivated(x$1: java.awt.event.WindowEvent) {}
      def windowDeiconified(x$1: java.awt.event.WindowEvent) {}
      def windowIconified(x$1: java.awt.event.WindowEvent) {}
      def windowOpened(x$1: java.awt.event.WindowEvent) {}
    })
    f.setSize(initialSize._1 + 20, initialSize._2 + 50)
    f.add(canvas)
    f
  }
  var offset: (Int, Int) = (0, 0)
  var pos: Option[(Int, Int)] = None
  var lastClickTime = System.currentTimeMillis()
  var nextClickPromise = Promise[(Int, Int)]()
  def nextClick = {
    nextClickPromise = Promise[(Int, Int)]()
    if (pos.nonEmpty && System.currentTimeMillis() > lastClickTime + 100) {
      lastClickTime = System.currentTimeMillis()
      nextClickPromise.trySuccess(pos.get)
    }
    nextClickPromise.future
  }
  var pixelSize = 8
  lazy val canvas: Canvas = {
    val c = new Canvas() {
      var painted: Seq[Seq[(BigInt, BigInt)]] = Nil
      override def paint(g: Graphics) {
        painted = thePics.reverse
        val points = painted.flatten
        if (points.nonEmpty) {
          val minX = points.map(_._1).min
          val minY = points.map(_._2).min
          val maxX = points.map(_._1).max
          val maxY = points.map(_._2).max
          val width = (maxX - minX + 1).toInt
          val height = (maxY - minY + 1).toInt
	  if (width * pixelSize > 1600) pixelSize = 1600 / width
	  if (height * pixelSize > 1000) pixelSize = 1000 / height
          if (width * pixelSize > getSize.width || height * pixelSize > getSize.height) {
            frame.setSize(width * pixelSize + 20, height * pixelSize + 50)
            setSize(width * pixelSize, height * pixelSize)
          }
          if (minX < offset._1 || minY < offset._2) {
            offset = ((minX.toInt min offset._1),(minY.toInt min offset._2))
          }
          g.translate(-offset._1 * pixelSize, -offset._2 * pixelSize)
          val step = 255 / (painted.size + 1)
          for {
            (pic, index) <- painted.zipWithIndex
            col = step * (2 + index)
            color = new Color(col, col * 2 % 256, col * 3 %256)
            point <- pic
          } {
            val x = point._1.toInt * pixelSize
            val y = point._2.toInt * pixelSize
            g.setColor(color)
            g.drawLine(x, y, x, y)
            g.fillRect(x, y, pixelSize, pixelSize)
          }
        }
      }
    }
    val listener = new MouseListener() with MouseMotionListener {
      def register(e: MouseEvent) {
        val x = (e.getX() + (offset._1 * pixelSize)) / pixelSize
        val y = (e.getY() + (offset._2 * pixelSize)) / pixelSize
        pos = Some((x, y))
        if (System.currentTimeMillis() > lastClickTime + 100) {
          lastClickTime = System.currentTimeMillis()
          nextClickPromise.trySuccess((x, y))
        }
      }
      def mouseClicked(e: MouseEvent) {}
      def mouseEntered(e: MouseEvent) {}
      def mouseExited(e: MouseEvent) { pos = None }
      def mousePressed(e: MouseEvent) { register(e) }
      def mouseReleased(e: MouseEvent) { pos = None }
      def mouseMoved(e: MouseEvent) {}
      def mouseDragged(e: MouseEvent) { if (pos.nonEmpty) mouseClicked(e) }
    }
    c.addMouseListener(listener)
    c.addMouseMotionListener(listener)
    c.setBackground(new Color(0, 0, 0))
    c.setSize(initialSize._1, initialSize._2)
    c
  }

  def multidraw(pics: Seq[Seq[(BigInt, BigInt)]]) = {
    thePics = AST.strict(pics).asInstanceOf[Seq[Seq[(BigInt, BigInt)]]]
    frame.show()
    canvas.repaint()
    pics.map(draw).mkString("vvvv\n", "----\n", "^^^^\n")
  }
}
