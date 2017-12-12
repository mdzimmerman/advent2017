import scala.io.Source

/**
  * Created by mzimmerman on 12/12/17.
  */

case class Graph(input: List[String]) {
  val map = buildMap()

  def buildMap() = {
    val inputPat = """(\d+) <-> (.+)""".r

    input.flatMap(_ match {
        case inputPat(inStr, outStr) =>
          //println(s"[$inStr][$outStr]")
          val in = inStr.toInt
          val out = outStr.split(", ").map(_.toInt).toList
          //println(out)
          Some(in -> out)
        case _ => None
    }).toMap
  }

  def getGroups(): List[Set[Int]] = getGroups(List[Set[Int]](), map.keys.toSet)

  def getGroups(groups: List[Set[Int]], unvisited: Set[Int]): List[Set[Int]] = {
    if (unvisited.isEmpty)
      groups
    else {
      val groupNew = dfs(unvisited.head)
      getGroups(groups ++ List(groupNew), unvisited -- groupNew)
    }
  }

  def dfs(vertex: Int): Set[Int] = dfs(vertex, Set[Int]())

  def dfs(vertex: Int, visited: Set[Int]): Set[Int] = {
    if (visited.contains(vertex))
      visited
    else {
      val neighbors = map(vertex).filterNot(visited.contains)
      neighbors.foldLeft(visited + vertex)((vis, ver) => dfs(ver, vis))
    }
  }
}

val test = Graph(List(
  "0 <-> 2",
  "1 <-> 1",
  "2 <-> 0, 3, 4",
  "3 <-> 2, 4",
  "4 <-> 2, 3, 6",
  "5 <-> 6",
  "6 <-> 4, 5"
))

println(test.dfs(0).size)

val input = Graph(Source.fromFile("input.txt").getLines.toList)

println(input.dfs(0).size)
val inputGroups = input.getGroups()
for (g <- inputGroups) {
  println(g)
}
println(inputGroups.length)