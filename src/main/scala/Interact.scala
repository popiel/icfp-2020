package foo

import java.net._
import java.net.http._
import scala.annotation._
import scala.concurrent._
import scala.concurrent.duration._
import scala.math
import scala.util.control._

import AST._

object IO {
  var noisy = true

  val uri = URI.create("https://icfpc2020-api.testkontur.ru/aliens/send?apiKey=1340d7f0d5004d40a8b3083863167298")

  def send(data: Any): Any = {
    println("Sending: " + data)
    try {
      val request = HttpRequest.newBuilder
        .uri(uri)
        .version(HttpClient.Version.HTTP_1_1)
        .header("accept", "*/*")
        .POST(HttpRequest.BodyPublishers.ofString(Modulate.modulate(data)))
        .build
      val response = HttpClient.newHttpClient
        .send(request, HttpResponse.BodyHandlers.ofString)
      val status = response.statusCode
      if (status != HttpURLConnection.HTTP_OK) {
        println("Unexpected server response:")
        println("HTTP code: " + status)
        println("Response body: " + response.body)
        System.exit(2)
      }
      val (output, extra) = Modulate.demodulate(response.body.trim)
      if (extra != "") {
        println(s"Extra stuff in response: '$extra'")
      }
      println("Received: " + output)
      output
    } catch {
      case NonFatal(e) =>
        println("Unexpected server response:")
        e.printStackTrace(System.out)
        System.exit(1)
    }
  }

  def deepListify(a: Any): Any = a match {
    case (x, y) =>
      val (j, k) = (deepListify(x), deepListify(y))
      k match {
        case l: List[_] => j :: l
	case _ => (j, k)
      }
    case x => x
  }

  var lastModulation = "00"
  var lastHash = ""
  var lastClick = (0, 0)
  def store(state: Any) = {
    val s = AST.strict(state)
    val l = deepListify(s)
    val m = Modulate.modulate(s)
    val d = java.security.MessageDigest.getInstance("MD5").digest(m.getBytes)
    val h = d.map(x => ("0" + ((x + 256) % 256).toHexString).takeRight(2)).mkString

    val f = new java.io.File(s"states/$h")
    if (!f.exists) {
      val o = new java.io.FileWriter(f)
      try {
        val p = new java.io.PrintWriter(o)
	p.println(lastModulation)
	p.println(Modulate.modulate(lastClick))
	p.println("State: " + l)
	p.println("Click: " + lastClick)
	p.println("From: " + lastHash)
	p.flush()
      } finally {
        o.close()
      }
    }
    lastHash = h
    lastModulation = m

    if (noisy) println("State: " + l)
    if (noisy) println("Hash: " + h)
    s
  }
}

object Interact extends Interact("galaxy")

case class Interact(name: String) {
  val interpreter = new Interpreter()
  interpreter.runFile(name + ".txt")
  val protocol = interpreter.symbols(name)

  var state: Any = Nil

  def load(hash: String) = {
    val lines = scala.io.Source.fromFile("states/" + hash).getLines
    state = Modulate.demodulate(lines.next())._1
    val (x, y) = Modulate.demodulate(lines.next())._1.asInstanceOf[(BigInt, BigInt)]
    click(x, y)
  }

  def click(x: BigInt, y: BigInt): Any = {
    if (IO.noisy) println(s"Got click ($x, $y)")
    state = AST.interact(protocol, state, (x, y))
    state
  }

  @tailrec final def interact(n: Int = 0) {
    if (IO.noisy) println(s"Waiting for click $n")
    val next = Await.result(Drawing.nextClick, 10.minutes)
    IO.lastClick = next
    click(next._1, next._2)
    interact(n + 1)
  }
}
