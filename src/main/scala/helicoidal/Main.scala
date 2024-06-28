package helicoidal

import java.net.URI
import java.net.http.HttpRequest.BodyPublishers
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.{ HttpClient, HttpRequest }

import scala.annotation.tailrec
import scala.io.StdIn

object Client {
  def send(payload: String): String = {
    val body = Str.encode(payload)
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
    Str.decode(res.body())
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
    str.map(Human2Alien(_)).mkString("S", "", "")
}

object Main {
  def main(args: Array[String]): Unit = {
    // val s = Coder.decode("SB%,,/}!.$}7%,#/-%}4/}4(%}M#(//,}/&}4(%}</5.$}P!2)!\",%_~~<%&/2%}4!+).'}!}#/523%j}7%}35''%34}4(!4}9/5}(!6%}!},//+}!2/5.$l}S/5e2%}./7},//+).'}!4}4(%}u).$%8wl}N/}02!#4)#%}9/52}#/--5.)#!4)/.}3+),,3j}9/5}#!.}53%}/52}u%#(/w}3%26)#%l}@524(%2-/2%j}4/}+./7}(/7}9/5}!.$}/4(%2}345$%.43}!2%}$/).'j}9/5}#!.},//+}!4}4(%}u3#/2%\"/!2$wl~~;&4%2},//+).'}!2/5.$j}9/5}-!9}\"%}!$-)44%$}4/}9/52}&)234}#/523%3j}3/}-!+%}352%}4/}#(%#+}4()3}0!'%}&2/-}4)-%}4/}4)-%l}C.}4(%}-%!.4)-%j})&}9/5}7!.4}4/}02!#4)#%}-/2%}!$6!.#%$}#/--5.)#!4)/.}3+),,3j}9/5}-!9}!,3/}4!+%}/52}u,!.'5!'%y4%34wl~")
    // println(s"$s")

    // val s = Coder.decode("S'%4}).$%8")
    // println(s"$s")

    // val s = Coder.decode("SB%,,/}Q/2,$_")
    // println(s"$s")

    // val s = Coder.decode("SO.+./7.}).3425#4)/.n}`'%4}3#/2%\"/2$`")
    // println(s"$s")

    // println(Coder.encode("Hello World!"))
    // println(Coder.encode("get scoreboard"))

    repl()
  }

  @tailrec
  private def repl(prompt: String = "icfp> "): Unit = {
    StdIn.readLine(prompt) match {
      case ":q" => ()
      case other =>
        println(Client.send(other))
        repl()
    }
  }
}
