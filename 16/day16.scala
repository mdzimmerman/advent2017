import scala.annotation.tailrec
import scala.io.Source

sealed trait Move {
  def process(s: String): String
}

case class Spin(x: Int) extends Move {
  def process(s: String): String = {
    val y = s.length - x
    s.substring(y) + s.substring(0, y)
  }
}

case class Exchange(a: Int, b: Int) extends Move {
  def process(s: String): String = {
    val ca = s.charAt(a)
    val cb = s.charAt(b)
    s.zipWithIndex.map{ case (c, i) => {
      if (i == a)
        cb
      else if (i == b)
        ca
      else
        c
    }}.mkString("")
  }
}

case class Partner(a: Char, b: Char) extends Move {
  def process(s: String): String = s.zipWithIndex.map{ case (c, i) => {
    if (c == a)
      b
    else if (c == b)
      a
    else
      c
  }}.mkString("")
}

object Move {
  val spin     = """s(\d+)""".r
  val exchange = """x(\d+)/(\d+)""".r
  val partner  = """p(.)/(.)""".r

  def parse(s: String): Option[Move] = s match {
    case spin(x)        => Some(Spin(x.toInt))
    case exchange(a, b) => Some(Exchange(a.toInt, b.toInt))
    case partner(a, b)  => Some(Partner(a.charAt(0), b.charAt(0)))
    case _ => None
  }

  @tailrec
  final def dance(moves: List[Move], input: String, debug: Boolean = false): String = {
    if (moves.isEmpty)
      input
    else {
      val m = moves.head
      if (debug)
        println(s"$input - $m")
      dance(moves.tail, m.process(input), debug)
    }
  }

  def danceRepeat(moves: List[Move], input: String, total: Int): String = danceRepeat(0, Map(), moves, input, total)

  @tailrec
  final def danceRepeat(repeat: Int, seen: Map[String, Int], moves: List[Move], input: String, total: Int): String = {
    if (repeat == total)
      input
    else {
      //if (left % 1000 == 0)
      val prev = if (seen.contains(input)) seen(input) else -1
      println(s"$input - $repeat ($prev)")
      danceRepeat(repeat + 1, seen + (input -> repeat), moves, dance(moves, input), total)
    }
  }

  def buildDance(input: String, output: String): List[Move] = buildDance(input, output, List())

  @tailrec
  final def buildDance(input: String, output: String, moves: List[Move]): List[Move] = {
    if (input == output)
      moves.reverse
    else {
      val a = commonPrefixLength(input, output)
      val b = input.indexOf(output(a))
      val exchange = Exchange(a, b)
      buildDance(exchange.process(input), output, exchange :: moves)
    }
  }

  def commonPrefixLength(s: String, t: String): Int = {
    (s,t).zipped.takeWhile(c => c._1 == c._2).unzip._1.size
  }
}

val testMoves = List(
  "s1",
  "x3/4",
  "pe/b"
).flatMap(Move.parse)
val testInput = "abcde"
println(Move.dance(testMoves, testInput, true))

val inputMoves = Source.fromFile("input.txt").getLines().mkString("").split(",").flatMap(Move.parse).toList
val input = "abcdefghijklmnop"
val output = Move.dance(inputMoves, input)
println(input)

val newMoves = Move.buildDance(input, output)
println(Move.dance(newMoves, input, true))
println(output)

println(s"old = ${inputMoves.length}")
println(s"new = ${newMoves.length}")

println(Move.danceRepeat(inputMoves, input, 100))
val rem = 1000000000 % 60
println(rem)