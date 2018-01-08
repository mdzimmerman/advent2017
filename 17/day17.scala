import scala.annotation.tailrec

case class Spinlock(seq: Seq[Int], spin: Int) {
  lazy val size = seq.length

  def next(): Spinlock = {
    val offset = spin % size
    val rotseq = rotate(offset)
    Spinlock(Seq(size) ++ rotseq.tail ++ Seq(rotseq.head), spin)
  }

  def rotate(n: Int): Seq[Int] = seq.slice(n, size) ++ seq.slice(0, n)
}

@tailrec
def spin(times: Int, s: Spinlock): Spinlock = {
  if (times == 0)
    s
  else {
    if (times % 1000 == 0)
      println(times)
    spin(times - 1, s.next())
  }
}

val testOut = spin(2017, Spinlock(Seq(0), 3))
println(testOut.seq.slice(0, 5))

val inputOut = spin(2017, Spinlock(Seq(0), 348))
println(inputOut.seq.slice(0, 5))

var pos = 0
val step = 348
var neighbor = 0
for (i <- 1 to 50000000) {
  pos = (pos + step) % i
  if (pos == 0) {
    neighbor = i
  }
  pos += 1
}
println(neighbor)
