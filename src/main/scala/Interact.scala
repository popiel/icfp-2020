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
  val uri = URI.create("https://icfpc2020-api.testkontur.ru/aliens/send?apiKey=1340d7f0d5004d40a8b3083863167298")

  def send(data: Any): Any = {
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
      output
    } catch {
      case NonFatal(e) =>
        println("Unexpected server response:")
        e.printStackTrace(System.out)
        System.exit(1)
    }
  }

  def store(state: Any) {
  }
}

object Interact extends Interact("galaxy")

case class Interact(name: String) {
  val interpreter = new Interpreter()
  interpreter.runFile(name + ".txt")
  val protocol = interpreter.symbols(name)

  var state: Any = Nil

  def click(x: BigInt, y: BigInt): Any = {
    state = extract[List[Any]](interpreter.parse(Map("protocol" -> protocol, "state" -> state, "vector" -> (x, y)),
      "ap ap ap interact protocol state vector"
    )).head
    state
  }

  @tailrec final def interact(x: BigInt, y: BigInt, n: Int = 0) {
    click(x, y)
    println(s"Waiting for click $n")
    val next = Await.result(Drawing.nextClick, 10 minutes)
    println(s"Got click $next")
    interact(next._1, next._2, n + 1)
  }
}
