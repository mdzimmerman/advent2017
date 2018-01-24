import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

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

object State extends Enumeration {
  type State = Value
  val Clean, Weakened, Infected, Flagged = Value
}

import State._

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
  val map: mutable.Map[Point, State] = parseInit(init)

  def getXMin: Int = map.keys.map(_.x).min
  def getXMax: Int = map.keys.map(_.x).max
  def getYMin: Int = map.keys.map(_.y).min
  def getYMax: Int = map.keys.map(_.y).max

  def get(p: Point): State = {
    if (!map.contains(p))
      map(p) = Clean
    map(p)
  }

  def get(x: Int, y: Int): State = get(Point(x, y))

  def set(p: Point, value: State): State = {
    map(p) = value
    value
  }

  def toggle(p: Point): State = {
    val newval = get(p) match {
      case Infected => Clean
      case Clean    => Infected
    }
    set(p, newval)
    newval
  }

  override def toString: String = {
    val buffer = 3
    val xMin = getXMin
    val xMax = getXMax
    val yMin = getYMin
    val yMax = getYMax
    (yMin-buffer to yMax+buffer).map(y => {
      (xMin-buffer to xMax+buffer).map(x =>
        if (x < xMin || x > xMax || y < yMin || y > yMax)
          '.'
        else
          get(x, y) match {
            case Infected => '#'
            case Weakened => 'W'
            case Flagged => 'F'
            case Clean => '.'
          }
      ).mkString("")
    }).mkString("\n")
  }

  def toImage: BufferedImage = {
    val buffer = 3
    val xMin = getXMin
    val xMax = getXMax
    val yMin = getYMin
    val yMax = getYMax
    val w = xMax - xMin + 1 + 2 * buffer
    val h = yMax - yMin + 1 + 2 * buffer
    val out = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    for (y <- yMin - buffer to yMax + buffer)
      for (x <- xMin - buffer to xMax + buffer) {
        val xi = x + Math.abs(xMin) + buffer
        val yi = y + Math.abs(yMin) + buffer
        if (x < xMin || x > xMax || y < yMin || y > yMax)
          out.setRGB(xi, yi, 0xffffff)
        else
          get(x, y) match {
            case Infected => out.setRGB(xi, yi, 0x000000)
            case Weakened => out.setRGB(xi, yi, 0x666666)
            case Flagged  => out.setRGB(xi, yi, 0xaaaaaa)
            case Clean    => out.setRGB(xi, yi, 0xffffff)
          }
      }
    out
  }

  private def parseInit(init: List[String]): mutable.Map[Point, State] = {
    val xMin = -(init.head.length / 2)
    val yMin = -(init.length / 2)
    val map = mutable.Map[Point, State]()
    for ((row, j) <- init.zipWithIndex ; (char, i) <- row.zipWithIndex) {
      val pos = Point(xMin + i, yMin + j)
      val value = char match {
        case '#' => Infected
        case '.' => Clean
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
    val (newdir: Dir, newinfected: Int) = grid.get(carrier.posit) match {
      case Infected =>
        grid.set(carrier.posit, Clean)
        (carrier.dir.right, infected)
      case Clean =>
        grid.set(carrier.posit, Infected)
        (carrier.dir.left, infected+1)
    }
    val newposit = carrier.posit.move(newdir)
    move(left-1, newinfected, grid, Carrier(newposit, newdir))
  }
}

@tailrec
def move2(left: Int, infected: Int, grid: Grid, carrier: Carrier, debug: Int = 0): Int = {
  if (debug > 0) println(s"$left $infected $carrier")
  if (debug > 1) println(grid)
  if (left == 0) {
    infected
  }
  else {
    val (newdir: Dir, newinfected: Int) = grid.get(carrier.posit) match {
      case Clean =>
        grid.set(carrier.posit, Weakened)
        (carrier.dir.left, infected)
      case Weakened =>
        grid.set(carrier.posit, Infected)
        (carrier.dir, infected + 1)
      case Infected =>
        grid.set(carrier.posit, Flagged)
        (carrier.dir.right, infected)
      case Flagged =>
        grid.set(carrier.posit, Clean)
        (carrier.dir.right.right, infected)
    }
    val newposit = carrier.posit.move(newdir)
    move2(left - 1, newinfected, grid, Carrier(newposit, newdir), debug)
  }
}

val test = List(
  "..#",
  "#..",
  "...")
val input = Source.fromFile("input.txt").getLines().toList

val testGrid1 = new Grid(test)
println(testGrid1)
move(70, 0, testGrid1, Carrier(Point(0, 0), North))
println(testGrid1)

val inputGrid1 = new Grid(input)
println(inputGrid1)
move(10000, 0, inputGrid1, Carrier(Point(0, 0), North))
println(inputGrid1)

val testGrid2 = new Grid(test)
println(move2(10, 0, new Grid(test), Carrier(Point(0, 0), North), 2))

val testGrid3 = new Grid(test)
println(move2(10000000, 0, testGrid3, Carrier(Point(0, 0), North), 0))
ImageIO.write(testGrid3.toImage, "png", new File("test2.png"))

val inputGrid2 = new Grid(input)
println(move2(10000000, 0, inputGrid2, Carrier(Point(0, 0), North), 0))
ImageIO.write(inputGrid2.toImage, "png", new File("input2.png"))