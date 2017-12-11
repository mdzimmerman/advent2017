import scala.annotation.tailrec
import scala.io.Source

implicit class RichSeq[T](s: Seq[T]) {
  def rotate(n: Int): Seq[T] = {
    val npos = if (n < 0) s.length + n else n
    s.slice(npos, s.length) ++ s.slice(0, npos)
  }
}

def process(s: Seq[Int], start: Int, length: Int): Seq[Int] = {
  val (a, b) = s.rotate(start).splitAt(length)
  (a.reverse ++ b).rotate(-start)
}

def processAll(s: Seq[Int], lengths: List[Int]): Seq[Int] = processAll(s, 0, 0, lengths)

@tailrec
def processAll(s: Seq[Int], start: Int, skip: Int, lengths: List[Int]): Seq[Int] = {
  println(s)
  if (lengths.isEmpty)
    s
  else {
    val sNew = process(s, start, lengths.head)
    processAll(sNew, (start + lengths.head + skip) % s.length, skip + 1, lengths.tail)
  }
}

val test = Vector(0, 1, 2, 3, 4)
val testLengths = List(3, 4, 1, 5)
processAll(test, testLengths)

val input = (0 to 255).toVector
val inputLengths = Source.fromFile("input.txt").getLines().mkString("").split(",").map(_.toInt).toList
println(input)
println(inputLengths)
val out = processAll(input, inputLengths)
println(out(0) * out(1))