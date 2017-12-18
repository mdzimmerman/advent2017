import scala.collection.mutable
import scala.io.Source

/**
  * Created by mzimmerman on 12/18/17.
  */
sealed trait Value
case class Register(name: String) extends Value
case class Literal(n: Long)       extends Value

sealed trait Instruction
case class Snd(x: Value)              extends Instruction
case class Set(x: Register, y: Value) extends Instruction
case class Add(x: Register, y: Value) extends Instruction
case class Mul(x: Register, y: Value) extends Instruction
case class Mod(x: Register, y: Value) extends Instruction
case class Rcv(x: Register)           extends Instruction
case class Jgz(x: Value, y: Value)    extends Instruction

object Instruction {
  val register = """[a-z]"""
  val literal  = """-?\d+"""
  val value    = s"""$register|$literal"""
  val registerPattern = s"""($register)""".r
  val literalPattern  = s"""($literal)""".r

  object AsValue {
    def unapply(s: String): Option[Value] = s match {
      case registerPattern(name) => Some(Register(name))
      case literalPattern(n)     => Some(Literal(n.toLong))
      case _ => None
    }
  }

  object AsRegister {
    def unapply(s: String): Option[Register] = s match {
      case registerPattern(name) => Some(Register(name))
      case _ => None
    }
  }

  val snd = s"""snd ($value)""".r
  val set = s"""set ($register) ($value)""".r
  val add = s"""add ($register) ($value)""".r
  val mul = s"""mul ($register) ($value)""".r
  val mod = s"""mod ($register) ($value)""".r
  val rcv = s"""rcv ($register)""".r
  val jgz = s"""jgz ($value) ($value)""".r

  def apply(s: String): Option[Instruction] = s match {
    case snd(AsValue(x))                => Some(Snd(x))
    case set(AsRegister(x), AsValue(y)) => Some(Set(x, y))
    case add(AsRegister(x), AsValue(y)) => Some(Add(x, y))
    case mul(AsRegister(x), AsValue(y)) => Some(Mul(x, y))
    case mod(AsRegister(x), AsValue(y)) => Some(Mod(x, y))
    case rcv(AsRegister(x))             => Some(Rcv(x))
    case jgz(AsValue(x), AsValue(y))    => Some(Jgz(x, y))
    case _ => None
  }
}

case class Program(instructions: Seq[Instruction]) {
  var pos: Int = 0
  val registers = mutable.Map[String, Long]()
  var last: Long = 0
  init()

  def getValue(v: Value): Long = v match {
    case Register(name) => registers.getOrElseUpdate(name, 0L)
    case Literal(n)     => n
  }

  def init(): Unit = {
    pos = 0
    last = 0
    registers.clear()
  }

  def run(): Unit = {
    while (pos >= 0 && pos < instructions.length) {
      println(s"""[$pos ${instructions(pos)} $registers""")
      instructions(pos) match {
        case Snd(x) =>
          last = getValue(x)
          println(s"playing $last")
          pos += 1
        case Set(x, y) =>
          registers(x.name) = getValue(y)
          pos += 1
        case Add(x, y) =>
          registers(x.name) = getValue(x) + getValue(y)
          pos += 1
        case Mul(x, y) =>
          registers(x.name) = getValue(x) * getValue(y)
          pos += 1
        case Mod(x, y) =>
          registers(x.name) = getValue(x) % getValue(y)
          pos += 1
        case Rcv(x) =>
          if (getValue(x) != 0) {
            println(s"recovering $last")
            pos = instructions.length // jump to end to terminate
          } else {
            pos += 1
          }
        case Jgz(x, y) =>
          pos = if (getValue(x) > 0) pos + getValue(y).toInt else pos + 1
      }
    }
  }

  def print(): Unit = {
    for (i <- instructions)
      println(i)
  }
}

case class Process(pid: Int, instructions: Seq[Instruction]) {
  var other: Option[Process] = None
  val input = mutable.Queue[Long]()
  var stopped: Boolean = false
  var blocked: Boolean = false
  var pos: Int = 0
  val registers = mutable.Map[String, Long]()
  var last: Long = 0
  init()

  def isRunning: Boolean = !stopped && !blocked

  def getValue(v: Value): Long = v match {
    case Register(name) => registers.getOrElseUpdate(name, 0L)
    case Literal(n) => n
  }

  def init(): Unit = {
    pos = 0
    last = 0
    registers.clear()
    registers("p") = pid
  }

  def runStep(): Int = {
    if (pos >= 0 && pos < instructions.length) {
      //println(s"""[$pid ($pos) ${instructions(pos)} $registers $input""")
      instructions(pos) match {
        case Snd(x) =>
          other.get.input.enqueue(getValue(x))
          pos += 1
          1
        case Set(x, y) =>
          registers(x.name) = getValue(y)
          pos += 1
          0
        case Add(x, y) =>
          registers(x.name) = getValue(x) + getValue(y)
          pos += 1
          0
        case Mul(x, y) =>
          registers(x.name) = getValue(x) * getValue(y)
          pos += 1
          0
        case Mod(x, y) =>
          registers(x.name) = getValue(x) % getValue(y)
          pos += 1
          0
        case Rcv(x) =>
          if (input.isEmpty) {
            blocked = true
          } else {
            blocked = false
            registers(x.name) = input.dequeue()
            pos += 1
          }
          0
        case Jgz(x, y) =>
          pos = if (getValue(x) > 0) pos + getValue(y).toInt else pos + 1
          0
      }
    } else {
      stopped = true
      0
    }
  }

  def print(): Unit = {
    for (i <- instructions)
      println(i)
  }
}

case class Program2(instructions: Seq[Instruction]) {
  val p0 = Process(0, instructions)
  val p1 = Process(1, instructions)
  p0.other = Some(p1)
  p1.other = Some(p0)

  def run() = {
    var send0 = 0
    var send1 = 0
    while (p0.isRunning || p1.isRunning) {
      send0 += p0.runStep()
      send1 += p1.runStep()
    }
    println(s"p0 sent = $send0")
    println(s"p1 sent = $send1")
  }

}

val test = Program(List(
  "set a 1",
  "add a 2",
  "mul a a",
  "mod a 5",
  "snd a",
  "set a 0",
  "rcv a",
  "jgz a -1",
  "set a 1",
  "jgz a -2"
).flatMap(Instruction(_)))

println("--- Test program ---")
test.print()
println("--- Test execution ---")
test.run()
println()

println("--- Input program ---")
val input = Source.fromFile("input.txt").getLines().flatMap(Instruction(_)).toSeq
val input1 = Program(input)
input1.print()
println("--- Input execution ---")
input1.run()

println()
println("### Part 2 ###")
println("--- Test ---")
val test2 = Program2(List(
  "snd 1",
  "snd 2",
  "snd p",
  "rcv a",
  "rcv b",
  "rcv c",
  "rcv d"
).flatMap(Instruction(_)))
test2.run()
println()

println("--- Input ---")
val input2 = Program2(input)
input2.run()
