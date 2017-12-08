import scala.annotation.tailrec
import scala.io.Source

/**
  * Created by mzimmerman on 12/8/17.
  */

sealed trait Act {
  val reg: String
  val n: Int

  def eval(regvalue: Int): Int
}
case class Inc(reg: String, n: Int) extends Act {
  override def eval(regvalue: Int): Int = regvalue + n
}
case class Dec(reg: String, n: Int) extends Act {
  override def eval(regvalue: Int): Int = regvalue - n
}

sealed trait Cond {
  val reg: String
  val n: Int

  def isValid(regvalue: Int): Boolean
}
case class   Gt(reg: String, n: Int) extends Cond {
  override def isValid(regvalue: Int): Boolean = regvalue > n
}
case class   Lt(reg: String, n: Int) extends Cond {
  override def isValid(regvalue: Int): Boolean = regvalue < n
}
case class   Eq(reg: String, n: Int) extends Cond {
  override def isValid(regvalue: Int): Boolean = regvalue == n
}
case class   Ne(reg: String, n: Int) extends Cond {
  override def isValid(regvalue: Int): Boolean = regvalue != n
}
case class GtEq(reg: String, n: Int) extends Cond {
  override def isValid(regvalue: Int): Boolean = regvalue >= n
}
case class LtEq(reg: String, n: Int) extends Cond {
  override def isValid(regvalue: Int): Boolean = regvalue <= n
}

case class Instr(act: Act, cond: Cond)

case class Program(input: List[String]) {
  val instructions = Program.parse(input)

  def evaluate(): (Int, Int) = evaluate(instructions, Map[String, Int](), 0)

  @tailrec
  final def evaluate(instructions: List[Instr], registers: Map[String, Int], max: Int): (Int, Int) = {
    //println(s"$max : $registers")
    if (instructions.isEmpty) {
      (registers.values.max, max)
    } else {
      val in = instructions.head
      val condRegVal = registers.getOrElse(in.cond.reg, 0)
      val registersNew: Map[String, Int] = if (in.cond.isValid(condRegVal)) {
        val actRegVal = registers.getOrElse(in.act.reg, 0)
        registers + (in.cond.reg -> condRegVal) + (in.act.reg -> in.act.eval(actRegVal))
      } else {
        registers + (in.cond.reg -> condRegVal)
      }
      val maxNew = registersNew.values.max
      evaluate(instructions.tail, registersNew, if (maxNew > max) maxNew else max)
    }
  }
}

object Program {
  object ToAct {
    val pattern = s"""([a-z]+) (inc|dec) (-?[0-9]+)""".r

    def unapply(s: String): Option[Act] = s match {
      case pattern(reg, typ, num) => typ match {
        case "inc" => Some(Inc(reg, num.toInt))
        case "dec" => Some(Dec(reg, num.toInt))
        case _ => None
      }
      case _ => None
    }
  }

  object ToCond {
    val pattern = s"""([a-z]+) (>|<|>=|<=|==|!=) (-?[0-9]+)""".r

    def unapply(s: String): Option[Cond] = s match {
      case pattern(reg, op, num) => op match {
        case ">"  => Some(  Gt(reg, num.toInt))
        case "<"  => Some(  Lt(reg, num.toInt))
        case ">=" => Some(GtEq(reg, num.toInt))
        case "<=" => Some(LtEq(reg, num.toInt))
        case "==" => Some(  Eq(reg, num.toInt))
        case "!=" => Some(  Ne(reg, num.toInt))
        case _ => None
      }
      case _ => None
    }
  }

  def parse(in: List[String]): List[Instr] = in.flatMap(parseLine)

  def parseLine(s: String): Option[Instr] = {
    val pattern = s"""(.+) if (.+)""".r
    s match {
      case pattern(ToAct(act), ToCond(cond)) => Some(Instr(act, cond))
      case _ => None
    }
  }
}

println("### Test ###")
val test = Program(List(
  "b inc 5 if a > 1",
  "a inc 1 if b < 5",
  "c dec -10 if a >= 1",
  "c inc -20 if c == 10"))
println(test.evaluate())

println("### Input ###")
val input = Program(Source.fromFile("input.txt").getLines().toList)
println(input.evaluate())