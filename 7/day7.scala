import scala.io.Source

/**
  * Created by mzimmerman on 12/7/17.
  */

case class Node(name: String, weight: Int, children: List[String]) {
  var parent: Option[String] = None
  var childrenNodes: Option[List[Node]] = None
  var weightCumulative: Int = 0

  override def toString = s"Node(name=$name, weight=$weight, children=$children, parent=$parent)"
}

case class Graph(nodesString: List[String]) {
  val WithoutChildren = """(.+) \(([0-9]+)\)""".r
  val WithChildren = """(.+) \(([0-9]+)\) -> (.+)""".r

  val nodes = nodesString.flatMap(_ match {
    case WithoutChildren(name, value) =>
      Some(Node(name, value.toInt, List()))
    case WithChildren(name, value, children) =>
      Some(Node(name, value.toInt, children.split(", ").toList))
    case _ => None
  }).map(n => n.name -> n).toMap

  // set parents
  for ((name, node) <- nodes) {
    node.childrenNodes = Some(node.children.map(nodes(_)))
    for (k <- node.children) {
      nodes(k).parent = Some(name)
    }
  }

  def print() = {
    for ((name, node) <- nodes) {
      println(node)
    }
  }

  def getRoots(): List[Node] = {
    nodes.values.filter(_.parent.isEmpty).toList
  }
}

println("## Test (part 1) ##")
val testInput =
  """
    |pbga (66)
    |xhth (57)
    |ebii (61)
    |havc (66)
    |ktlj (57)
    |fwft (72) -> ktlj, cntj, xhth
    |qoyq (66)
    |padx (45) -> pbga, havc, qoyq
    |tknk (41) -> ugml, padx, fwft
    |jptl (61)
    |ugml (68) -> gyxo, ebii, jptl
    |gyxo (61)
    |cntj (57)
  """.stripMargin.trim.split("""\n""").toList
val test = Graph(testInput)
test.print()
println(test.getRoots())

println("## Input (part 1) ##")
val input = Graph(Source.fromFile("input.txt").getLines().toList)
input.print()
println(input.getRoots())