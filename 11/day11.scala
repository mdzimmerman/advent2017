import scala.annotation.tailrec
import scala.io.Source

/**
  * Created by mzimmerman on 12/11/17.
  *
  * This was helpful: http://keekerdc.com/2011/03/hexagon-grids-coordinate-systems-and-distance-calculations/
  */

sealed trait Dir

object Dir {
  def parse(s: String): Option[Dir] = s match {
    case "nw" => Some(NW)
    case "n"  => Some(N)
    case "ne" => Some(NE)
    case "se" => Some(SE)
    case "s"  => Some(S)
    case "sw" => Some(SW)
    case _ => None
  }

  def parseList(s: String): List[Dir] = s.split(",").flatMap(parse).toList
}

case object NW extends Dir
case object N  extends Dir
case object NE extends Dir
case object SE extends Dir
case object S  extends Dir
case object SW extends Dir

case class Cell(x: Int, y: Int) {
  val z = -(x+y)
  lazy val distOrigin = dist(Cell(0, 0))

  def next(dir: Dir): Cell = {
    dir match {
      case NW => Cell(x-1, y+1)
      case N  => Cell(x,   y+1)
      case NE => Cell(x+1, y)
      case SE => Cell(x+1, y-1)
      case S  => Cell(x,   y-1)
      case SW => Cell(x-1, y)
    }
  }

  def moveTo(dir: String): (Cell, Int) = moveTo(Dir.parseList(dir))

  def moveTo(dir: List[Dir]): (Cell, Int) = moveTo(dir, this, 0)

  @tailrec
  final def moveTo(dir: List[Dir], curr: Cell, maxDist: Int): (Cell, Int) =
    if (dir.isEmpty)
      (curr, maxDist)
    else {
      val next = curr.next(dir.head)
      val nextDist = Cell(0, 0).dist(next)
      moveTo(dir.tail, next, if (nextDist > maxDist) nextDist else maxDist)
    }

  def dist(that: Cell): Int = List(that.x - this.x, that.y - this.y, that.z - this.z).max
}

val c0 = Cell(0, 0)
val input = Source.fromFile("input.txt").getLines().mkString("")
val test2 = List(
  "ne,ne,ne",
  "ne,ne,sw,sw",
  "ne,ne,s,s",
  "se,sw,se,sw,sw"
)
for (t <- test2) {
  val c1 = c0.moveTo(t)
  println(s"$t => $c1 (${c0.dist(c1._1)}, ${c1._2})")
}

val out = c0.moveTo(input)
println(s"<input> => $out (${c0.dist(out._1)}, ${out._2})")