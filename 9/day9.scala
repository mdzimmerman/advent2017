object State extends Enumeration {
  type State = Value
  val InGroup, InGarbage, InExclude = Value
}
import State._

case class Output(depth: Int, total: Int, garbage: Int)

import scala.annotation.tailrec
import scala.io.Source

def process(s: String): Output = process(s, InGroup, Output(0, 0, 0)): Output

@tailrec
def process(s: String, state: State, out: Output): Output = {
  if (s.isEmpty) {
    //println(s"[] $state $depth $total")
    out
  } else {
    val curr = s.head
    //println(s"[$curr] $state $depth $total")
    val (newState, newOut) = state match {
      case InGroup =>
        curr match {
          case '{' => (InGroup, Output(out.depth+1, out.total, out.garbage))
          case '}' => (InGroup, Output(out.depth-1, out.total + out.depth, out.garbage))
          case '<' => (InGarbage, out)
          case _   => (InGroup, out)
        }
      case InGarbage =>
        curr match {
          case '!' => (InExclude, out)
          case '>' => (InGroup, out)
          case _   => (InGarbage, Output(out.depth, out.total, out.garbage+1))
        }
      case InExclude =>
        curr match {
          case _   => (InGarbage, out) // after this char back to garbage
        }
      }
    process(s.tail, newState, newOut)
  }
}

val garbageTest = List(
  "<>",
  "<random characters>",
  "<<<<>",
  "<{!>}>",
  "<!!>",
  "<!!!>>",
  """<{o"i!a,<{i<a>"""
)

for (s <- garbageTest) {
  println(s"--- $s ---")
  println(process(s))
}

val parseTest = List(
  "{}",
  "{{{}}}",
  "{{},{}}",
  "{{{},{},{{}}}}",
  "{<{},{},{{}}>}",
  "{<a>,<a>,<a>,<a>}",
  "{{<a>},{<a>},{<a>},{<a>}}",
  "{{<!>},{<!>},{<!>},{<a>}}"
)

for (s <- parseTest) {
  println(s"--- $s ---")
  println(process(s))
}

val input = Source.fromFile("input.txt").getLines.mkString("")
println("### Input ###")
println(process(input))