package helicoidal

import java.net.URI
import java.net.http.HttpRequest.BodyPublishers
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.{ HttpClient, HttpRequest }

import scala.annotation.tailrec
import scala.io.StdIn

object Client {
  def send(payload: String): String = {
    val body = "S" + Str.encode(payload)
    println(s">>> $body")
    val client = HttpClient.newHttpClient()
    val req = HttpRequest.newBuilder
      .uri(new URI("https://boundvariable.space/communicate"))
      .header("Authorization", s"Bearer ${sys.env("ICFP_TOKEN")}")
      .POST(BodyPublishers.ofString(body))
      .build()

    val res = client.send(req, BodyHandlers.ofString())
    println(s"<<< $res")
    println(s"<<< ${res.body()}")
    res.body()
  }
}

object Str {
  private val chars =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

  private val AlienToHuman = chars.toList

  private val Human2Alien =
    chars.zipWithIndex.map((c, i) => c -> (i + 33).toChar).toMap

  def decode(str: String): String =
    if str.isEmpty
    then str
    else str.map(c => AlienToHuman(c.toInt - 33)).mkString

  def encode(str: String): String =
    str.map(Human2Alien(_)).mkString
}

object Main {
  def main(args: Array[String]): Unit =
    repl(prompt = "icfp> ")

  @tailrec
  private def repl(prompt: String): Unit = {
    StdIn.readLine(prompt) match {
      case ":q" => ()
      case line => eval(line); repl(prompt)
    }
  }

  private def eval(line: String): Unit = {
    val response = Client.send(line)

    if (line == "get language_test") {
      val expr = Parser.parse(response)
      println(s"EXPR: $expr")
      val result = Eval.eval(expr)
      println(s"RESULT: $result")
    } else {
      println(Str.decode(response.drop(1)))
    }
  }
}
