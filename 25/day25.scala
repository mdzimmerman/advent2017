import scala.collection.mutable

/**
  * Created by mzimmerman on 1/11/18.
  */

class Tape {
  private var currentPos = 0
  private val tape: mutable.Map[Int, Int] = mutable.Map[Int, Int]()
  tape += (currentPos -> 0)

  def current: Int = currentPos

  def move(offset: Int): Int = {
    currentPos += offset
    get(currentPos)
  }

  def get(pos: Int): Int = {
    if (!tape.contains(pos)) tape += (pos -> 0)
    tape(pos)
  }

  def get(): Int = get(currentPos)

  def set(value: Int): Unit = tape += (currentPos -> value)

  def checksum: Int = tape.values.count(_ == 1)

  override def toString: String = {
    val min = tape.keys.min
    val max = tape.keys.max
    val s = (min to max).map(i => {
      val v = tape(i)
      if (i == currentPos) s"[$v]" else s" $v "
    }).mkString("")
    s"Tape($s)"
  }
}

case class Action(write: Int, move: Int, next: String)

case class State(name: String, v0: Action, v1: Action)

case class Machine(initState: String, steps: Int, states: List[State], debug: Boolean = false) {
  val stateMap: Map[String, State] = states.map(s => s.name -> s).toMap
  val tape = new Tape()
  var currentState: String = initState

  def run(): Int = {
    if (debug) println(tape)
    for (i <- 1 to steps) {
      val state = stateMap(currentState)
      tape.get match {
        case 0 =>
          tape.set(state.v0.write)
          tape.move(state.v0.move)
          currentState = state.v0.next
        case 1 =>
          tape.set(state.v1.write)
          tape.move(state.v1.move)
          currentState = state.v1.next
        case _ =>
          throw new Exception("invalid value")
      }
      if (debug) println(tape)
    }
    tape.checksum
  }

  def graphviz(): String = {
    val sb = new StringBuilder()
    sb.append("digraph {\n")
    for (s <- states) {
      sb.append(s"""    ${s.name} -> ${s.v0.next} [ label = "0" ]\n""")
      sb.append(s"""    ${s.name} -> ${s.v1.next} [ label = "1" ]\n""")
    }
    sb.append("}\n")
    sb.mkString
  }
}

val test = Machine("A", 6, List(
  State("A", Action(1, 1, "B"), Action(0, -1, "B")),
  State("B", Action(1, -1, "A"), Action(1, 1, "A"))),
  debug=true
)
println(test.run())

val input = Machine("A", 12656374, List(
  State("A", Action(1,  1, "B"), Action(0, -1, "C")),
  State("B", Action(1, -1, "A"), Action(1, -1, "D")),
  State("C", Action(1,  1, "D"), Action(0,  1, "C")),
  State("D", Action(0, -1, "B"), Action(0,  1, "E")),
  State("E", Action(1,  1, "C"), Action(1, -1, "F")),
  State("F", Action(1, -1, "E"), Action(1,  1, "A"))
))
//println(input.run())
println(input.graphviz())