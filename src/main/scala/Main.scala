@main def f: Unit =
  val nfa = NFA()
  nfa.addState("q0")
  nfa.addState("q1")
  nfa.addState("q2")
  nfa.addState("q3")
  nfa.setInitialState("q0")
  nfa.setEndingStates(Seq("q3"))
  nfa.appendTransition("q0", "q1", Matcher(Some('a')))
  nfa.appendTransition("q1", "q2", Matcher(Some('b')))
  nfa.appendTransition("q2", "q3", Matcher(Some('b')))
  nfa.appendTransition("q2", "q3", Matcher(None))

  println(nfa.compute("abbbbbb")); // True
  println(nfa.compute("aabbbbbb")); // False
  println(nfa.compute("ab")); // True
  println(nfa.compute("a"));
