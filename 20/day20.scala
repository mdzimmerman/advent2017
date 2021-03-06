import scala.annotation.tailrec
import scala.io.Source

case class Vec(x: Int, y: Int, z: Int) {
  def +(that: Vec): Vec = Vec(x + that.x, y + that.y, z + that.z)

  def distance(that: Vec) = Math.abs(x - that.x) + Math.abs(y - that.y) + Math.abs(z - that.z)
}

val origin = Vec(0, 0, 0)

case class Particle(id: Int, posit: Vec, veloc: Vec, accel: Vec) {
  lazy val distance = posit.distance(origin)

  def next(): Particle = {
    val velocNew = veloc + accel
    val positNew = posit + velocNew
    Particle(id, positNew, velocNew, accel)
  }
}

object Particle {
  val num = "-?[0-9]+"
  val pattern = s"""p=<($num),($num),($num)>, v=<($num),($num),($num)>, a=<($num),($num),($num)>""".r

  def parse(n: Int, s: String): Option[Particle] = s match {
    case pattern(px, py, pz, vx, vy, vz, ax, ay, az) =>
      Some(Particle(
        n,
        Vec(px.toInt, py.toInt, pz.toInt),
        Vec(vx.toInt, vy.toInt, vz.toInt),
        Vec(ax.toInt, ay.toInt, az.toInt)))
    case _ =>
      None
  }
}

@tailrec
def run(n: Int, ps: List[Particle]): List[Particle] = {
  println(ps.length+" "+ps.sortWith(_.distance < _.distance).map(_.id).take(10))
  if (n == 0)
    ps
  else {
    //val psNew = ps.map(_.next())
    val psNew = ps.map(_.next()).foldLeft[Map[Vec,List[Particle]]](Map()){case(map, p) => {
      if (map.contains(p.posit))
        map + (p.posit -> (p :: map(p.posit)) )
      else
        map + (p.posit -> List(p))
    }}.filter{case(posit, particles) => particles.size == 1}.values.flatten.toList
    run(n-1, psNew)
  }
}

val testInput = List(
  "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>",
  "p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>"
)
val test = testInput.zipWithIndex.flatMap{case (s, i) => {
  Particle.parse(i, s)
}}

run(100, test)

val input = Source.fromFile("input.txt").getLines().zipWithIndex.flatMap{case (s, i) => {
  Particle.parse(i, s)
}}.toList

run(1000, input)
