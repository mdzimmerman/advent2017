import scala.io.Source

/**
  * Created by mzimmerman on 12/8/17.
  */

val test = List(
  "5 1 9 5",
  "7 5 3",
  "2 4 6 8"
)

val test2 = List(
  "5 9 2 8",
  "9 4 7 3",
  "3 8 6 5"
)

val input = Source.fromFile("input.txt").getLines.toList

def checksum1(in: Seq[Int]): Int = in.max - in.min

def checksum2(in: Seq[Int]): Int = {
  for (i <- 0 to in.length-2; j <- i+1 to in.length-1) {

    val (max, min) = if (in(j) > in(i)) (in(j), in(i)) else (in(i), in(j))
    if (max > min && (max % min) == 0) {
      val div = max/min
      println(s"[$max/$min == $div]")
      return div
    }
  }
  println("[returning 0]")
  0
}

def checksumAll(in: List[String], checksum: Seq[Int] => Int): Int =
  in.map(line => {
    val list = line.trim.split("""\s+""").map(_.toInt).toSeq
    checksum(list)
  }).sum

println("### Test #1 ###")
println(checksumAll(test, checksum1))

println("### Input #1 ###")
println(checksumAll(input, checksum1))

println("### Test #2 ###")
println(checksumAll(test2, checksum2))

println("### Input #2 ###")
println(checksumAll(input, checksum2))
