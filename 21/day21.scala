/**
  * Created by mzimmerman on 1/15/18.
  */

case class Grid(array: Array[Array[Char]]) {
  val size = array.size

  def print(): Unit =
    for (row <- array)
      println(row.mkString(""))

  //def flipVert: Grid = {
  //}
}

object Grid {
  def parse(s: String): Grid = {
    val lines = s.split("/")
    Grid(lines.map(_.toCharArray).toArray)
  }
}

val start = Grid.parse("-#-/--#/###")
start.print()