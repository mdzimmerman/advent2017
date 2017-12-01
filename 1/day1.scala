import scala.io.Source

/**
  * Created by mzimmerman on 12/1/17.
  */

def captcha(s: String): Int = captcha(s.toSeq, 1)

def captcha2(s: String): Int = captcha(s.toSeq, s.length/2)

def captcha(s: Seq[Char], offset: Int): Int = {
  var sum = 0
  for (i <- s.indices) {
    val next = (i + offset) % s.length
    //println(s"$i $next : [${s(i)}][${s(next)}]")
    if (s(i) == s(next))
      //println(seq(i).toString.toInt)
      sum += s(i).toString.toInt
  }
  sum
}

println("--Part #1--")
println("Tests")
val tests = List(
  "1122",
  "1111",
  "1234",
  "91212129"
)
for (t <- tests)
  println(s"$t => ${captcha(t)}")
println()

println("Input")
val input = Source.fromFile("input.txt").getLines.mkString("").trim()
println(s"<input> => ${captcha(input)}")
println()

println("--Part #2--")
println("Tests")
val tests2 = List(
  "1212",
  "1221",
  "123425",
  "123123",
  "12131415"
)
for (t <- tests2)
  println(s"$t => ${captcha2(t)}")
println()

println("Input")
println(s"<input> => ${captcha2(input)}")