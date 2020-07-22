import java.net._
import java.net.http._

import foo._

import scala.util.control.NonFatal

object Main {
  def main(args: Array[String]) {
    val interact = Interact("galaxy")
    val click = if (args.size > 0) {
      interact.load(args(0))
    } else {
      interact.click(0, 0)
    }
    interact.interact()
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
