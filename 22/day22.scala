import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

/**
  * Created by mzimmerman on 1/22/18.
  */

sealed trait Dir {
  def right: Dir
  def left: Dir
}
case object North extends Dir {
  def right = East
  def left  = West
}
case object East  extends Dir {
  def right = South
  def left  = North
}
case object South extends Dir {
  def right = West
  def left  = East
}
case object West  extends Dir {
  def right = North
  def left  = South
}

case class Point(x: Int, y: Int) {
  def move(dir: Dir): Point = dir match {
    case North => Point(x, y-1)
    case East  => Point(x+1, y)
    case South => Point(x, y+1)
    case West  => Point(x-1, y)
  }
}

case class Carrier(posit: Point, dir: Dir)

class Grid(init: List[String]) {
  val map: mutable.Map[Point, Boolean] = parseInit(init)

  def getXMin: Int = map.keys.map(_.x).min
  def getXMax: Int = map.keys.map(_.x).max
  def getYMin: Int = map.keys.map(_.y).min
  def getYMax: Int = map.keys.map(_.y).max

  def get(p: Point): Boolean = {
    if (!map.contains(p))
      map(p) = false
    map(p)
  }

  def get(x: Int, y: Int): Boolean = get(Point(x, y))

  def set(p: Point, value: Boolean): Boolean = {
    map(p) = value
    value
  }

  def toggle(p: Point): Boolean = set(p, !get(p))

  override def toString: String = {
    val xMin = getXMin
    val xMax = getXMax
    val yMin = getYMin
    val yMax = getYMax
    (yMin to yMax).map(y => {
      (xMin to xMax).map(x =>
        if (get(x, y)) '#' else '.'
      ).mkString("")
    }).mkString("\n")
  }

  private def parseInit(init: List[String]): mutable.Map[Point, Boolean] = {
    val xMin = -(init.head.length / 2)
    val yMin = -(init.length / 2)
    val map = mutable.Map[Point, Boolean]()
    for ((row, j) <- init.zipWithIndex ; (char, i) <- row.zipWithIndex) {
      val pos = Point(xMin + i, yMin + j)
      val value = char match {
        case '#' => true
        case '.' => false
        case _ => throw new Exception("invalid char")
      }
      map(pos) = value
    }
    map
  }
}

@tailrec
def move(left: Int, infected: Int, grid: Grid, carrier: Carrier): Int = {
  println(s"$left $infected $carrier")
  if (left == 0)
    infected
  else {
    val newdir: Dir = if (grid.get(carrier.posit)) carrier.dir.right else carrier.dir.left
    val newinfected = if (grid.toggle(carrier.posit)) infected + 1 else infected
    val newposit = carrier.posit.move(newdir)
    move(left-1, newinfected, grid, Carrier(newposit, newdir))
  }
}

val test = new Grid(List(
  "..#",
  "#..",
  "..."
))
println(test)
move(70, 0, test, Carrier(Point(0, 0), North))
println(test)

val input = new Grid(Source.fromFile("input.txt").getLines().toList)
println(input)
move(10000, 0, input, Carrier(Point(0, 0), North))
println(input)
