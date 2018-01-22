import scala.annotation.tailrec
import scala.io.Source

sealed trait Dir
object N extends Dir
object S extends Dir
object W extends Dir
object E extends Dir

case class Pos(row: Int, col: Int, direction: Dir) {
  def next(d: Dir): Pos = {
    d match {
      case N => Pos(row-1, col, d)
      case E => Pos(row, col+1, d)
      case S => Pos(row+1, col, d)
      case W => Pos(row, col-1, d)
    }
  }

  def next(): Pos = next(direction)

  override def toString: String = {
    val dir = direction match {
      case N => "N"
      case S => "S"
      case W => "W"
      case E => "E"
    }
    s"Pos($row, $col, $dir)"
  }
}

case class Graph(lines: List[String]) {
  val array: Array[Array[Char]] = lines.map(_.toCharArray).toArray

  val startCol = array(0).indexOf('|')
  val start = Pos(0, startCol, S)

  def getChar(p: Pos): Char = {
    if (p.row >= 0 && p.row < array.length) {
      val row = array(p.row)
      if (p.col >= 0 && p.col < row.length)
        row(p.col)
      else ' '
    } else ' '
  }

  def next(p: Pos): Option[Pos] = {
    val c = getChar(p)
    c match {
      case ' ' => None
      case '+' => p.direction match {
        case N | S =>
          if (getChar(p.next(E)) == ' ')
            Some(p.next(W))
          else
            Some(p.next(E))
        case E | W =>
          if (getChar(p.next(N)) == ' ')
            Some(p.next(S))
          else
            Some(p.next(N))
      }
      case _ => Some(p.next)
    }
  }

  def traverse(): String = traverse(start, List(), 0)

  @tailrec
  final def traverse(pos: Pos, seen: List[Char], n: Int): String = {
    println(s"$n ${getChar(pos)} $pos")
    next(pos) match {
      case None =>
        seen.reverse.mkString("")
      case Some(next) =>
        val cnext = getChar(next)
        val newseen = if (cnext >= 'A' && cnext <= 'Z') cnext :: seen else seen
        traverse(next, newseen, n+1)
    }
  }
}

val test = Graph(Source.fromFile("test.txt").getLines().toList)
println(test.traverse())

val input = Graph(Source.fromFile("input.txt").getLines().toList)
println(input.traverse())
