import scala.annotation.tailrec
import scala.collection.mutable

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

//println(countBits(test))
//println(countBits(input))

val testArray = buildArray(test)
val inputArray = buildArray(input)

def printarray(array: Array[Array[Int]]): Unit =
  for (j <- 0 to 7) {
    for (i <- 0 to 7) {
      print(array(j)(i) match {
        case 1  => "#"
        case -1 => "o"
        case _  => "-"
      })
    }
    println()
  }

case class Point(x: Int, y: Int) {
  def add(that: Point): Point = {
    Point(this.x + that.x, this.y + that.y)
  }
}

val directions = List(Point(-1, 0), Point(0, -1), Point(1, 0), Point(0, 1))

def neighbors(array: Array[Array[Int]], p: Point) = {
  for {
    d <- directions
    np = p.add(d)
    if np.x >= 0 && np.x <= 127 && np.y >= 0 && np.y <= 127 && array(np.y)(np.x) == 1
  } yield np
}

def dfs(array: Array[Array[Int]], start: Point): Unit = {
  val s = mutable.Stack[Point]()
  s.push(start)
  while (s.nonEmpty) {
    val p = s.pop()
    //println(p)
    if (array(p.y)(p.x) == 1) {
      array(p.y)(p.x) = -1
      for (np <- neighbors(array, p))
        s.push(np)
    }
  }
}

def countGroups(array: Array[Array[Int]]) = {
  var x = 0
  var y = 0
  var groups = 0
  while (y <= 127) {
    if (array(y)(x) == 1) {
      dfs(array, Point(x, y))
      groups += 1
    }
    x += 1
    if (x > 127) {
      x = 0
      y += 1
    }
  }
  groups
}

printarray(testArray)
println(countGroups(testArray))
println(countGroups(inputArray))
//dfs(testArray, Point(0, 0))
//printarray(testArray)
