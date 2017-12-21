import scala.annotation.tailrec

/**
  * Created by mzimmerman on 12/18/17.
  */

object Knot {
  implicit class RichSeq[T](s: Seq[T]) {
    def rotate(n: Int): Seq[T] = {
      val npos = if (n < 0) s.length + n else n
      s.slice(npos, s.length) ++ s.slice(0, npos)
    }
  }

  private def knot(s: Seq[Int], start: Int, length: Int): Seq[Int] = {
    val (a, b) = s.rotate(start).splitAt(length)
    (a.reverse ++ b).rotate(-start)
  }

  case class RoundState(seq: Seq[Int], start: Int, skip: Int)

  private def calcRound(s: Seq[Int], lengths: List[Int], debug: Boolean = false): RoundState =
    calcRound(RoundState(s, 0, 0), lengths, debug)

  @tailrec
  private final def calcRound(r: RoundState, lengths: List[Int], debug: Boolean): RoundState = {
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

  @tailrec
  private final def calcSparseHash(round: RoundState, lengths: List[Int], n: Int): RoundState = {
    if (n == 64)
      round
    else {
      val roundNew = calcRound(round, lengths, false)
      calcSparseHash(roundNew, lengths, n+1)
    }
  }

  private def calcDenseHash(in: Seq[Int]): String = {
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
}


def hexToBinary(hex: String): String = {
  "%16s".format(Integer.parseInt(hex, 16).toBinaryString).replace(' ', '0')
}

def countBits(prefix: String): Int = {
  (0 to 127).map(i => {
    val hash = Knot.hash(prefix + "-" + i.toString)
    val line = hash.sliding(4, 4).toList.map(hexToBinary).mkString.replace('0', '-').replace('1', '#')
    if (i <= 7)
      println(line.substring(0, 8))
    hash.sliding(4, 4).toList.map(Integer.parseUnsignedInt(_, 16)).map(Integer.bitCount).sum
  }).sum
}

def buildArray(prefix: String): Array[Array[Int]] = {
  (0 to 127).map(i => {
    val hash = Knot.hash(prefix + "-" + i.toString)
    val row = hash.sliding(4, 4).toList.map(hexToBinary).mkString.toArray.map(_.toString.toInt)
    row
  }).toArray
}

val test  = "flqrgnkx"
val input = "ffayrhll"

println(countBits(test))
println(countBits(input))

val testArray = buildArray(test)
for (j <- 0 to 7) {
  for (i <- 0 to 7)
    print(if (testArray(j)(i) == 1) "#" else "-")
  println()
}
