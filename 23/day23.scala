import scala.collection.mutable
import scala.io.Source

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

  val set = s"""set ($register) ($value)""".r
  val sub = s"""sub ($register) ($value)""".r
  val mul = s"""mul ($register) ($value)""".r
  val jnz = s"""jnz ($value) ($value)""".r

  def parse(s: String): Option[Instruction] = s match {
    case set(AsRegister(x), AsValue(y)) => Some(Set(x, y))
    case sub(AsRegister(x), AsValue(y)) => Some(Sub(x, y))
    case mul(AsRegister(x), AsValue(y)) => Some(Mul(x, y))
    case jnz(AsValue(x), AsValue(y))    => Some(Jnz(x, y))
    case _ => None
  }
}

class Program(instructions: Seq[Instruction]) {
  val register = mutable.Map[String, Long]()
  var pointer = 0
  init()

  def init(): Unit = {
    register.clear()
    for (c <- List("a", "b", "c", "d", "e", "f", "g", "h"))
      register(c) = 0
    pointer = 0
  }

  def get(name: String) = register(name)

  def getValue(v: Value): Long = v match {
    case reg: Register => register(reg.name)
    case lit: Literal  => lit.n
  }

  def set(name: String, n: Long) = register(name) = n

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

val input = new Program(Source.fromFile("input.txt").getLines().flatMap(Instruction.parse).toSeq)
//input.run()
input.init()
//input.set("a", 1)
input.run()
