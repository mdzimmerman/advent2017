import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

/**
  * Created by mzimmerman on 12/6/17.
  */

case class Memory(banks: Seq[Int]) {
  val size = banks.length

  def cycle(): Memory = {
    val bankNew = new mutable.ArrayBuffer[Int](size)
    bankNew ++= banks
    val max = banks.max
    val i0 = banks.indexOf(max)
    bankNew(i0) = 0
    for (n <- 1 to max) {
      bankNew((i0 + n) % size) += 1
    }
    Memory(bankNew)
  }

  override def toString: String = {
    "Memory("+banks.mkString(" ")+")"
  }
}

object Memory {
  def rebalance(m: Memory): Int = rebalance(m, Map[Memory, Int](), 0)

  @tailrec
  def rebalance(m: Memory, history: Map[Memory, Int], n: Int): Int = {
    println(m)
    if (history.contains(m)) {
      val nLast = history(m)
      println(s"$n - $nLast = ${n - nLast}")
      n
    } else {
      val mNew = m.cycle()
      rebalance(mNew, history + (m -> n), n + 1)
    }
  }
}

val test = Memory(Seq(0, 2, 7, 0))
println(Memory.rebalance(test))

val s = Source.fromFile("input.txt").getLines().toList.head
val input = Memory(s.split("\\s+").map(_.toInt))
println(Memory.rebalance(input))