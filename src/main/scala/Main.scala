import java.net._
import java.net.http._

import foo._

import scala.util.control.NonFatal

object Main {
  def main(args: Array[String]) {
    val interact = Interact("galaxy")
    if (args.size > 1) {
      args(1) match {
        case "brute" =>
	  IO.noisy = false
	  Drawing.showing = false
	  val used = scala.collection.mutable.Map[Int, Int]()
	  val pairs = for {
	    a <- 0 to 63
	    x1 = a / 8
	    y1 = a % 8
	    b <- a + 1 to 64
	    if (!(used contains b) && !(used contains a))
	    x2 = b / 8
	    y2 = b % 8
	  } yield {
	    if (b == 255) println (s"scanned ($x1, $y1)")
	    interact.load("208dc00f61410204c24da096809697a5")
	    interact.click(x1 * 6 + 2, y1 * 6 + 2)
	    val result = IO.deepListify(AST.strict(interact.click(x2 * 6 + 2, y2 * 6 + 2)))
            if (AST.extract[Seq[_]](AST.extract[Seq[_]](result).apply(1)).last != Nil) {
	      
	      used(a) = b
	      used(b) = a
	      println(s"Found ($x1, $y1) ($x2, $y2)")
	      List(((x1, y1), (x2, y2)))
	    } else Nil
	  }
	  println(pairs.toSeq.flatten)
      }
    }
    if (args.size > 0) {
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
