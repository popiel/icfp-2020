import java.net._
import java.net.http._

import foo._

import scala.util.control.NonFatal

object Main {
  def main(args: Array[String]) {
    val interact = Interact("galaxy")
    val click = if (args.size > 0) {
// Boot sequence to get symbols crunched
      interact.click(0,0)
      interact.click(0,0)
      interact.click(0,0)
      interact.click(0,0)
      interact.click(0,0)
      interact.click(0,0)
      interact.click(0,0)
      interact.click(0,0)
      interact.click(8,4)
      interact.click(2,-7)
      interact.click(2,-8)
      interact.click(3,6)
      interact.click(0,-14)
      interact.click(-3,10)
      interact.click(-3,9)
      interact.click(-3,10)
      interact.click(-4,10)
      interact.click(9,-2)
      interact.click(9,-3)
      interact.click(-4,10)
      interact.click(1,4)

      val lines = scala.io.Source.fromFile("states/" + args(0)).getLines
      interact.state = Modulate.demodulate(lines.next())._1
      Modulate.demodulate(lines.next())._1.asInstanceOf[(BigInt, BigInt)]
    } else {
      (BigInt(0), BigInt(0))
    }
    interact.interact(click._1, click._2)
  }

/*
  try {
    val serverUrl = args(0)
    val playerKey = args(1)
    println("ServerUrl: " + serverUrl + "; PlayerKey: " + playerKey)
    val request = HttpRequest.newBuilder
      .uri(URI.create(serverUrl))
      .version(HttpClient.Version.HTTP_1_1)
      .POST(HttpRequest.BodyPublishers.ofString(playerKey))
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
    println("Server response: " + response.body)
  } catch {
    case NonFatal(e) =>
      println("Unexpected server response:")
      e.printStackTrace(System.out)
      System.exit(1)
  }
  */
}
