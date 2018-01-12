import scala.io.Source

/**
  * Created by mzimmerman on 1/12/18.
  */

case class Component(a: Int, b: Int) {
  def contains(n: Int): Boolean = a == n || b == n

  def other(n: Int): Int = if (n == a) b else a

  def strength: Int = a + b

  override def toString(): String = s"$a/$b"
}

object Component {
  val input = """(\d+)/(\d+)""".r

  def parse(s: String): Option[Component] = s match {
    case input(a, b) => Some(Component(a.toInt, b.toInt))
    case _ => None
  }
}

case class Bridge(components: List[Component] = Nil, endpoint: Int = 0) {
  def strength: Int = components.map(_.strength).sum

  def length: Int = components.length

  def connect(c: Component): Option[Bridge] =
    if (c.contains(endpoint))
      Some(Bridge(c :: components, c.other(endpoint)))
    else
      None

  override def toString() = components.reverse.mkString("--")
}

def getBridges(current: Bridge, components: Set[Component]): Iterator[Bridge] = {
  val bridges = components.toIterator.flatMap(current.connect)
  if (bridges.isEmpty)
    Iterator(current)
  else
    bridges.flatMap(connected => getBridges(connected, components - connected.components.head))
}

val test = List(
  "0/2",
  "2/2",
  "2/3",
  "3/4",
  "3/5",
  "0/1",
  "10/1",
  "9/10").flatMap(Component.parse).toSet

for (b <- getBridges(Bridge(), test))
  println(b)
println(getBridges(Bridge(), test).map(_.strength).max)

val input = Source.fromFile("input.txt").getLines().flatMap(Component.parse).toSet
println(input)
println(getBridges(Bridge(), input).map(_.strength).max)
println(getBridges(Bridge(), input).map(b => (b.length, b.strength)).max)