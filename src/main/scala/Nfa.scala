import scala.collection.mutable.HashMap
import scala.collection.StringView
type Transition = (Matcher, State)

class State(val name: String = "") {
  var transitions = Vector[Transition]()
  def prependTransition(t: Transition) =
    transitions = t +: transitions
  def appendTransition(t: Transition) =
    transitions = transitions :+ t
}

// None is eps
class Matcher(label: Option[Char]) {
  def matches(c: Char) =
    label match
      case None => true
      case Some(inner) => c == inner
  
  def isEpsilon = label == None
}

class NFA {
  var stateMap = HashMap[String, State]()
  var initialState = String()
  var endingStates = Set[String]()
  def setInitialState(stateName: String) = initialState = stateName
  def setEndingStates(stateNames: Seq[String]) = endingStates = stateNames.toSet
  def addState(stateName: String) =
    stateMap(stateName) = State(stateName)
  def prependTransition(from: String, to: String, matcher: Matcher) =
    stateMap(from).prependTransition((matcher, stateMap(to)))
  def appendTransition(from: String, to: String, matcher: Matcher) =
    stateMap(from).appendTransition((matcher, stateMap(to)))

  type Frame = (Int, State)
  case class Memory(epsilonVisited: Vector[String])
  def dfs(stringView: StringView, frame: Frame, memory: Memory): Boolean =
    val (position, state) = frame
    if endingStates contains state.name then return true
    val input = if position < stringView.length then stringView(position) else 0
    val transitionsToExecute =
      state.transitions
        .filter((matcher, toState) =>
          matcher.matches(input) &&
          !(matcher.isEpsilon && memory.epsilonVisited.contains(toState.name)))
    transitionsToExecute.exists((matcher, toState) =>
      val newMemory = memory.copy(
        epsilonVisited = if matcher.isEpsilon then Vector[String]()
        else memory.epsilonVisited :+ toState.name)
      val toPosition = position + (if matcher.isEpsilon then 0 else 1)
      val nextFrame = (toPosition, toState)
      dfs(stringView, nextFrame, newMemory)
    )

  def compute(string: String): Boolean =
    var initialFrame = (0, stateMap(initialState))
    var initialMemory = Memory(Vector[String]())
    dfs(StringView(string), initialFrame, initialMemory)
}
