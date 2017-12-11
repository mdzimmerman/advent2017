import scala.annotation.tailrec
import scala.io.Source

implicit class RichSeq[T](s: Seq[T]) {
  def rotate(n: Int): Seq[T] = {
    val npos = if (n < 0) s.length + n else n
    s.slice(npos, s.length) ++ s.slice(0, npos)
  }
}

def knot(s: Seq[Int], start: Int, length: Int): Seq[Int] = {
  val (a, b) = s.rotate(start).splitAt(length)
  (a.reverse ++ b).rotate(-start)
}

case class RoundState(seq: Seq[Int], start: Int, skip: Int)

def calcRound(s: Seq[Int], lengths: List[Int], debug: Boolean = false): RoundState = calcRound(RoundState(s, 0, 0), lengths, debug)

@tailrec
def calcRound(r: RoundState, lengths: List[Int], debug: Boolean): RoundState = {
  if (debug)
    println(r)
  if (lengths.isEmpty)
    r
  else {
    val rNew = RoundState(
      knot(r.seq, r.start, lengths.head),
      (r.start + lengths.head + r.skip) % r.seq.length,
      r.skip + 1)
    calcRound(rNew, lengths.tail, debug)
  }
}

def part1(lengths: List[Int]): Int = {
  val input = (0 to 255).toVector
  val out = calcRound(input, lengths)
  out.seq(0) * out.seq(1)
}

@tailrec
def calcSparseHash(round: RoundState, lengths: List[Int], n: Int): RoundState = {
  if (n == 64)
    round
  else {
    val roundNew = calcRound(round, lengths, false)
    calcSparseHash(roundNew, lengths, n+1)
  }
}

def calcDenseHash(in: Seq[Int]): String = {
  in.sliding(16, 16).map(_.reduce(_ ^ _).formatted("%02x")).mkString("")
}

def hash(s: String): String = {
  val seed = List(17, 31, 73, 47, 23)
  val sList = s.toList.map(_.toInt) ++ seed
  //println(s"sList=$sList")
  val initVector = (0 to 255).toVector
  val sparse = calcSparseHash(RoundState(initVector, 0, 0), sList, 0)
  calcDenseHash(sparse.seq)
}

println("--- Test #1 ---")
val test = Vector(0, 1, 2, 3, 4)
val testLengths = List(3, 4, 1, 5)
calcRound(test, testLengths, true)

println()
println("--- Part #1 ---")
//val input = (0 to 255).toVector
val input = Source.fromFile("input.txt").getLines().mkString("")
val inputLengths = input.split(",").map(_.toInt).toList
//println(input)
println(inputLengths)
val out = part1(inputLengths)
println(out)

println()
println("--- Test #2 ---")
val testDense = Vector(65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22)
println(s"$testDense => ${calcDenseHash(testDense)}")
println()
val test2 = List("", "AoC 2017", "1,2,3", "1,2,4")
for (t <- test2) {
  println(s"${hash(t)} <= $t")
}

println()
println("--- Part #2 ---")
println(hash(input))
