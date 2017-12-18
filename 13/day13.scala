import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

/**
  * Created by mzimmerman on 12/13/17.
  */

case class Firewall(layersList: List[String]) {
  val LayerPattern: Regex = """(\d+): (\d+)""".r

  val layers: Map[Int, Int] = layersList.flatMap({
    case LayerPattern(depth, range) => Some(depth.toInt -> range.toInt)
    case _ => None
  }).toMap

  val maxDepth: Int = layers.keys.max

  def getScannerPos(time: Int): Map[Int, Int] =
    layers.map({
      case (depth, range) =>
        val pass = time / (range-1)
        val pos0 = time % (range-1)
        val pos = if (pass % 2 == 0) pos0 else range-1-pos0
        depth -> pos
    })

  def traverse: Int = traverse(0, 0, 0)

  @tailrec
  final def traverse(currDepth: Int, time: Int, severity: Int): Int = {
    println(s"curr=$currDepth, time=$time, severity=$severity")
    if (currDepth > maxDepth) {
      severity
    } else {
      val scanners = getScannerPos(time)
      val severityNew = if (scanners.isDefinedAt(currDepth) && scanners(currDepth) == 0)
        severity + currDepth * layers(currDepth)
      else
        severity
      traverse(currDepth+1, time + 1, severityNew)
    }
  }

  def traverseClear(startTime: Int): Boolean = traverseClear(0, startTime)

  @tailrec
  final def traverseClear(depth: Int, time: Int): Boolean = {
    //println(s"$depth, $time")
    if (depth > maxDepth)
      true
    else {
      val scanners = getScannerPos(time)
      if (scanners.isDefinedAt(depth) && scanners(depth) == 0) {
        false
      } else {
        traverseClear(depth + 1, time + 1)
      }
    }
  }

  def findDelay(): Int = findDelay(0)

  @tailrec
  final def findDelay(startTime: Int): Int = {
    if (startTime % 10000 == 0) println(startTime)
    if (traverseClear(startTime))
      startTime
    else
      findDelay(startTime+1)
  }
}

val test = Firewall(List(
  "0: 3",
  "1: 2",
  "4: 4",
  "6: 4"
))

for (t <- 0 to 12) {
  println(s"$t -> ${test.getScannerPos(t)}")
}
println(test.traverse)
println(test.findDelay)

val input = Firewall(Source.fromFile("input.txt").getLines.toList)
println(input.traverse)
println(input.findDelay)