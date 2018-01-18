import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

/**
  * Created by mzimmerman on 1/15/18.
  */
case class Coord(r: Int, c: Int)

case class Grid(array: Vector[Char], size: Int) {
  def getIndex(r: Int, c: Int): Int = r * size + c

  def get(r: Int, c: Int): Char = array(getIndex(r, c))

  def print(): Unit =
    for (row <- 0 until size)
      println(array.subSequence(row * size, row * size + size))

  override def toString(): String =
    "Grid("+(0 until size).map(row => array.subSequence(row * size, row * size + size).toString).mkString("/")+")"

  def indices: IndexedSeq[Coord] =
    for (r <- 0 until size; c <- 0 until size) yield Coord(r, c)

  def indices(r0: Int, r1: Int, c0: Int, c1: Int): IndexedSeq[Coord] =
    for (r <- r0 until r1; c <- c0 until c1) yield Coord(r, c)

  def flipVert: Grid =
    Grid(indices.map{x => get(size-1-x.r, x.c)}.toVector, size)

  def flipHoriz: Grid =
    Grid(indices.map{x => get(x.r, size-1-x.c)}.toVector, size)

  def transpose: Grid =
    Grid(indices.map{x => get(x.c, x.r)}.toVector, size)

  def rotate: Grid = transpose.flipHoriz

  def transformations: List[Grid] = {
    val rot90 = rotate
    val rot180 = rot90.rotate
    val rot270 = rot180.rotate
    List(this, flipVert, rot90, rot90.flipVert, rot180, rot180.flipVert, rot270, rot270.flipVert)
  }

  def matches(that: Grid): Boolean =
    this.size == that.size && transformations.exists(_.array.mkString("") == that.array.mkString(""))

  def subgrid(r0: Int, c0: Int, subsize: Int): Grid = {
    val r1 = r0 + subsize
    val c1 = c0 + subsize
    Grid(indices(r0, r1, c0, c1).map{x => get(x.r, x.c)}.toVector, subsize)
  }

  def split(subsize: Int): Vector[Grid] = {
    (for (r <- 0 until size by subsize; c <- 0 until size by subsize) yield subgrid(r, c, subsize)).toVector
  }

  def enhance(rules: RuleList): Grid = {
    //println("building subgrids")
    val subgrids = size match {
      case n if n % 2 == 0 =>
        split(2)
      case n if n % 3 == 0 =>
        split(3)
      case _ =>
        throw new Exception("invalid grid size")
    }
    //println(subgrids)
    //println("replacing subgrids")
    val newSubgrids = subgrids.map(rules.replace)
   // println(newSubgrids)
    //println("joining subgrids")
    Grid.join(newSubgrids)
  }

  def countOn: Int = array.count(_ == '#')
}

object Grid {
  def parse(s: String): Option[Grid] = {
    val lines = s.split("/")
    val size = lines.length
    if (lines.count(_.length == size) == size)
      Some(Grid(lines.mkString("").toVector, size))
    else
      None
  }

  def iterateJoined(grids: Vector[Grid], dim: Int): IndexedSeq[Char] = {
    val size = grids.head.size
    for (gridRow <- 0 until dim;
         row <- 0 until size;
         gridCol <- 0 until dim;
         gridI = gridRow * dim + gridCol;
         col <- 0 until size)
      yield grids(gridI).get(row, col)
  }

  def join(grids: Vector[Grid]): Grid = {
    val dim = Math.round(Math.sqrt(grids.length)).toInt
    val size = grids.head.size
    Grid(Grid.iterateJoined(grids, dim).toVector, dim * size)
  }

  @tailrec
  final def applyEnhance(left: Int, grid: Grid, rules: RuleList, debug: Boolean = true): Grid = {
    if (debug) grid.print()
    println(s"$left: ${grid.countOn}")
    if (debug) println()
    if (left == 0)
      grid
    else
      applyEnhance(left-1, grid.enhance(rules), rules, debug)
  }
}

object AsGrid {
  def unapply(s: String): Option[Grid] = Grid.parse(s)
}

case class Rule(input: Grid, output: Grid) {
  def replace(grid: Grid): Option[Grid] =
    if (input.matches(grid))
      Some(output)
    else
      None

  override def toString(): String = s"Rule($input, $output)"
}

object Rule {
  val rulePattern = "(.+) => (.+)".r

  def parse(s: String): Option[Rule] = s match {
    case rulePattern(AsGrid(in), AsGrid(out)) => Some(Rule(in, out))
    case _ => None
  }
}

case class RuleList(rules: List[Rule]) {
  val cache: mutable.Map[Grid, Grid] = mutable.Map()

  def replace(grid: Grid): Grid =
    if (cache.contains(grid))
      cache(grid)
    else {
      val matching = rules.find(r => r.input.matches(grid)).head
      cache(grid) = matching.output
      matching.output
  }
}

val start = Grid.parse(".#./..#/###").get

println("--- test ---")
val testRules = RuleList(List(
  "../.# => ##./#../...",
  ".#./..#/### => #..#/..../..../#..#"
).flatMap(Rule.parse))
Grid.applyEnhance(2, start, testRules)

println("--- input ---")
val inputRules = RuleList(Source.fromFile("input.txt").getLines().flatMap(Rule.parse).toList)
Grid.applyEnhance(5, start, inputRules)

Grid.applyEnhance(18, start, inputRules, false)