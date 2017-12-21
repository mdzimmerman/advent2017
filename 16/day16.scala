sealed trait Move {
  def swap(s: String, x: Int, y: Int): String = {
    val cx = s.charAt(x)
    val cy = s.charAt(y)
    s.zipWithIndex.map{ case (c, i) => {
      if (i == x)
        s.charAt(y)
      else if (i == y)
        s.charAt(x)
      else
        c
    }}
  }

  def process(s: String): String
}

case class Spin(x: Int) extends Move {
  def process(s: String): String = {
    val y = s.length - x
    s.substring(y) + s.substring(0, y-1)
  }
}
case class Exchange(a: Int, b: Int) extends Move {
  def process(s: String): String = swap(s, a, b)
}
case class Partner(a: Char, b: Char) extends Move {
  def process(s: String): String = s // WRONG
}

object Move {
  val spin     = """s(\d+)""".r
  val exchange = """x(\d+)/(\d+)""".r
  val partner  = """p(.)/(.)""".r

  def parse(s: String): Option[Move] = s match {
    case spin(x)        => Some(Spin(x.toInt))
    case exchange(a, b) => Some(Exchange(a.toInt, b.toInt))
    case partner(a, b)  => Some(Partner(a.charAt(0), b.charAt(0)))
    case _ => None
  }
}

val test = List(
  "s1",
  "x3/4",
  "pe/b"
).flatMap(Move.parse)
println(test)