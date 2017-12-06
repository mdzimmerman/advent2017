import scala.io.Source

def solve(in: Seq[Int], f: Int => Int, debug: Int): Int = {
  val offsets = in.toArray
  var pos = 0
  var n = 0
  while (pos >= 0 && pos < offsets.length) {
    if ((n % debug) == 0) {
      print(s"$n ($pos)")
      if (offsets.length < 10)
        print(" [" + offsets.zipWithIndex.map { case (offset, i) => if (i == pos) s"($offset)" else s"$offset" }.mkString(" ") + "]")
      println()
    }
    val posNew = pos + offsets(pos)
    offsets(pos) = f(offsets(pos))
    pos = posNew
    n += 1
  }
  println(s"$n ($pos)")
  n
}

def solve1(in: Seq[Int], debug: Int = 1000): Int = solve(in, x => x+1, debug)

def solve2(in: Seq[Int], debug: Int = 1000): Int = solve(in, x => if (x >= 3) x-1 else x+1, debug)

println(solve1(Seq(0, 3, 0, 1, -3), 1))
println(solve2(Seq(0, 3, 0, 1, -3), 1))

val in = Source.fromFile("input.txt").getLines().map(_.toInt).toSeq

println(solve1(in, 10000))
println(solve2(in, 1000000))