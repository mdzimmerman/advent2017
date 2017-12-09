import scala.annotation.tailrec

case class Point(x: Int, y: Int) {
  def distance(): Int = distance(Point(0, 0))

  def distance(that: Point): Int = Math.abs(this.x - that.x) + Math.abs(this.y - that.y)

  def next(dir: Int): Point = (dir % 4) match {
    case 0 => Point(x+1, y)
    case 1 => Point(x, y+1)
    case 2 => Point(x-1, y)
    case 3 => Point(x, y-1)
  }

  def within(l: Limits): Boolean = x >= l.minX && x <= l.maxX && y >= l.minY && y <= l.maxY

  def neighbors: Seq[Point] = for (i <- x-1 to x+1 ; j <- y-1 to y+1 if i != x || j != y) yield Point(i, j)
}

case class Limits(minX: Int, minY: Int, maxX: Int, maxY: Int) {
  def min(a: Int, b: Int): Int = if (a <= b) a else b
  def max(a: Int, b: Int): Int = if (a >= b) a else b

  def extend(p: Point): Limits = Limits(min(minX, p.x), min(minY, p.y), max(maxX, p.x), max(maxY, p.y))
}

def getPoint(dest: Int): Point = getPoint(dest, 1, Point(0, 0), 0, Limits(0, 0, 0, 0))

@tailrec
def getPoint(dest: Int, curr: Int, currPoint: Point, dir: Int, limits: Limits): Point = {
  //println(s"$curr => $currPoint (dir=$dir)")
  if (curr == dest)
    currPoint
  else {
    val nextPoint = currPoint.next(dir)
    val (nextDir, nextLimits) = if (!nextPoint.within(limits))
      ((dir + 1) % 4, limits.extend(nextPoint))
    else
      (dir, limits)
    getPoint(dest, curr+1, nextPoint, nextDir, nextLimits)
  }
}

def getPoint2(limit: Int): Point = {
  val p0 = Point(0, 0)
  getPoint2(limit, Map(p0 -> 1), p0, 0, Limits(0, 0, 0, 0))
}

@tailrec
def getPoint2(limit: Int, value: Map[Point, Int], currPoint: Point, dir: Int, limits: Limits): Point = {
  println(s"$currPoint => ${value(currPoint)}")
  if (limit <= value(currPoint))
    currPoint
  else {
    val nextPoint = currPoint.next(dir)
    val (nextDir, nextLimits) = if (!nextPoint.within(limits))
      ((dir + 1) % 4, limits.extend(nextPoint))
    else
      (dir, limits)
    val nextValue = value + (nextPoint -> nextPoint.neighbors.flatMap(value.get).sum)
    getPoint2(limit, nextValue, nextPoint, nextDir, nextLimits)
  }
}

def getPointFast(in: Int): Point = {
  val root = Math.ceil(Math.sqrt(in.toDouble)).toInt
  val dist = root * root - in
  //println(s"root=$root dist=$dist")
  if ((root % 2) == 0) {
    val rootPoint = Point(-root / 2 + 1, root / 2)
    if (dist < root)
      Point(rootPoint.x + dist, rootPoint.y)
    else
      Point(rootPoint.x + (root-1), rootPoint.y - (dist - (root-1)))
  } else {
    val rootPoint = Point(root / 2, -root / 2)
    if (dist < root)
      Point(rootPoint.x - dist, rootPoint.y)
    else
      Point(rootPoint.x - (root-1), rootPoint.y + dist - (root-1))
  }
}

//val squares = List(1, 9, 25, 49, 4, 16, 36)
val test = List(1, 12, 23, 1024)

for (i <- test) {
  val point = getPoint(i)
  println(s"$i -> $point -> ${point.distance}")
}

val input = 289326
val inputPoint = getPoint(input)
//println(s"$input -> $inputPoint -> ${inputPoint.distance}")

for (p <- Point(0, 0).neighbors) {
  println(p)
}

getPoint2(input)