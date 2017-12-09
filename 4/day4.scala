import scala.io.Source

def isValid(s: String, matches: (String, String) => Boolean): Boolean = {
  val sList = s.split("""\s+""").toSeq
  for (i <- 0 to sList.length-2; j <- i+1 to sList.length-1)
    if (matches(sList(i), sList(j)))
      return false
  true
}

def matchAnagram(a: String, b: String): Boolean =
  a.toSeq.sorted.mkString("") == b.toSeq.sorted.mkString("")

def isValid1(s: String): Boolean = isValid(s, (a, b) => a == b)

def isValid2(s: String): Boolean = isValid(s, matchAnagram)

val test = List(
  "aa bb cc dd ee",
  "aa bb cc dd aa",
  "aa bb cc dd aaa")

val input = Source.fromFile("input.txt").getLines.toList

println("### Test #1 ###")
for (s <- test) {
  println(s"$s => ${isValid1(s)}")
}

println()
println("### Input #1 ###")
println(input.count(isValid1))

val test2 = List(
  "abcde fghij",
  "abcde xyz ecdab",
  "a ab abc abd abf abj",
  "iiii oiii ooii oooi oooo",
  "oiii ioii iioi iiio"
)

println()
println("### Test #2 ###")
for (s <- test2)
  println(s"$s => ${isValid2(s)}")

println()
println("### Input #2 ###")
println(input.count(isValid2))