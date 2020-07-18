package foo

import java.net._
import java.net.http._
import scala.math
import scala.util.control._

import AST._

object Interact extends Interact(Interpreter.protocol("galaxy"))

class Interact(protocol: AST[Any]) {
  val uri = URI.create("https://icfpc2020-api.testkontur.ru/aliens/send?apiKey=1340d7f0d5004d40a8b3083863167298")

  var state: Any = BigInt(0)

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

  def interact(state: Any, vector: Any): String = {
    println(s"Proto func: $protocol")
    val withState = Ap(protocol, Const(state))
    val withVector = Ap(withState, Const(vector))
    val chunk = withVector()
    println(s"Proto out: $chunk")
    val (flag, (newState, data)) = chunk.asInstanceOf[(Any, (Any, Any))]
    store(newState)
    if (flag == 0)
      Drawing.multidraw(data.asInstanceOf[Seq[Seq[(BigInt, BigInt)]]])
    else
      interact(newState, send(data))
  }

  def store(state: Any) {
  }
}
