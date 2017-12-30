case class Gen(start: Int, factor: Int, mult: Int) {
  var current: Long = start

  def next(): Long = {
    current = (current * factor) % 2147483647
    current
  }

  def next2(): Long = {
    while ((next() % mult) != 0) {
    }
    current
  }
}

val mask: Long = (1 << 16) - 1

def compare(aStart: Int, bStart: Int, n: Int = 40000000): Int = {
  val aGen = Gen(aStart, 16807, 4)
  val bGen = Gen(bStart, 48271, 8)

  var count = 0
  for (i <- 0 until n) {
    val a = aGen.next()
    val b = bGen.next()
    if ((a & mask) == (b & mask))
      count += 1
  }
  count
}

def compare2(aStart: Int, bStart: Int, n: Int = 5000000): Int = {
  val aGen = Gen(aStart, 16807, 4)
  val bGen = Gen(bStart, 48271, 8)

  var count = 0
  for (i <- 0 until n) {
    val a = aGen.next2()
    val b = bGen.next2()
    //println("%10d %10d".format(a, b))
    if ((a & mask) == (b & mask))
      count += 1
  }
  count
}

println(compare2(65, 8921))

println(compare2(699, 124))