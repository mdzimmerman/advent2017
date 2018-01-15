import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex

sealed trait Value
case class Register(name: String) extends Value
case class Literal(n: Long)       extends Value

sealed trait Instruction
case class Set(x: Register, y: Value) extends Instruction
case class Sub(x: Register, y: Value) extends Instruction
case class Mul(x: Register, y: Value) extends Instruction
case class Jnz(x: Value, y: Value)    extends Instruction

object Instruction {
  val register = """[a-z]"""
  val literal  = """-?\d+"""
  val value    = s"""$register|$literal"""
  val registerPattern: Regex = s"""($register)""".r
  val literalPattern: Regex = s"""($literal)""".r

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

  val set: Regex = s"""set ($register) ($value)""".r
  val sub: Regex = s"""sub ($register) ($value)""".r
  val mul: Regex = s"""mul ($register) ($value)""".r
  val jnz: Regex = s"""jnz ($value) ($value)""".r

  def parse(s: String): Option[Instruction] = s match {
    case set(AsRegister(x), AsValue(y)) => Some(Set(x, y))
    case sub(AsRegister(x), AsValue(y)) => Some(Sub(x, y))
    case mul(AsRegister(x), AsValue(y)) => Some(Mul(x, y))
    case jnz(AsValue(x), AsValue(y))    => Some(Jnz(x, y))
    case _ => None
  }
}

class Program(instructions: Seq[Instruction]) {
  val register: mutable.Map[String, Long] = mutable.Map[String, Long]()
  var pointer = 0
  init()

  def init(): Unit = {
    register.clear()
    for (c <- List("a", "b", "c", "d", "e", "f", "g", "h"))
      register(c) = 0
    pointer = 0
  }

  def get(name: String): Long = register(name)

  def getValue(v: Value): Long = v match {
    case reg: Register => register(reg.name)
    case lit: Literal  => lit.n
  }

  def set(name: String, n: Long): Unit = register(name) = n

  def run(): Unit = {
    var i = 0
    var mult = 0
    while (pointer >= 0 && pointer < instructions.size) {
      println(instructions(pointer)+" "+register)
      instructions(pointer) match {
        case Set(reg, value) =>
          register(reg.name) = getValue(value)
          pointer += 1
        case Sub(reg, value) =>
          register(reg.name) -= getValue(value)
          pointer += 1
        case Mul(reg, value) =>
          mult += 1
          register(reg.name) *= getValue(value)
          pointer += 1
        case Jnz(x, y) =>
          if (getValue(x) != 0)
            pointer += getValue(y).toInt
          else
            pointer += 1
      }
    }
    println(mult)
  }
}

def isPrime(n: Int): Boolean = n match {
  case 1 => false
  case 2 => true
  case 3 => true
  case x if x % 2 == 0 => false
  case x if x % 3 == 0 => false
  case x =>
    for (i <- 5 to Math.sqrt(n).floor.toInt) {
      if (n % i == 0 ) {
        return false
      }
    }
    true
}

def testProgram(a0: Int = 0): Int = {
  var a = a0
  var b = 0
  var c = 0
  var h = 0

  b = 67
  c = b
  if (a == 0) {
    //
  } else {
    b *= 100
    b += 100000
    c = b
    c += 17000
  }

  for (b0 <- b to c by 17) {
    println(b0)
    if (!isPrime(b0))
      h += 1
  }
  h
}

val input = new Program(Source.fromFile("input.txt").getLines().flatMap(Instruction.parse).toSeq)
input.init()
input.set("a", 0)
input.run()

println(testProgram(1))