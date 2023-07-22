import scala.collection.StringView

trait Parse[A] {
  def apply(str: String): Option[(A, String)]
  def map[B](transform: A => B): Parse[B] =
    apply(_).map((a, str) => (transform(a), str))
  def flatMap[B](transform: A => Parse[B]): Parse[B] =
    apply(_) match
      case Some(a, str) =>
        transform(a)(str)
      case None => None
  def filter(predicate: A => Boolean): Parse[A] =
    apply(_) match
      case Some(a, str) if predicate(a) => Some(a, str)
      case _ => None
  def zeroOrMore: Parse[Vector[A]] =
    str =>
      var matches = Vector[A]()
      var lastStr = str
      var lastParsed = apply(str)
      while (lastParsed != None) // How do I avoid this loop?
        lastParsed = apply(lastStr)
        lastParsed match
          case Some(a, newStr) =>
            matches = matches :+ a
            lastStr = newStr
          case None => None
      Some((matches, lastStr))
  def oneOrMore: Parse[Vector[A]] =
    zeroOrMore(_) match
      case Some((matches, newStr)) if !matches.isEmpty => Some((matches, newStr))
      case _ => None
  def optional: Parse[Option[A]] =
    in => apply(in) match
      case Some(a, out) => Some(Some(a), out)
      case None => Some(None, in)
}

object parseChar extends Parse[Char] {
  def apply(str: String) =
    if str.isEmpty() then None else Some(str.head, str.substring(1))
}

class parseString(mat: String) extends Parse[String] {
  def apply(str: String) =
    if str.startsWith(mat) then Some(mat, str.substring(mat.length))
    else None
}

val parseDigit = parseChar.filter(_.isDigit)
val parseNumber = parseDigit.oneOrMore
val parseRangeQuantifier = zip(
  parseString("{"), parseNumber, parseString(","), parseNumber, parseString("}")
)

def zip[A, B](parseA: Parse[A], parseB: Parse[B]): Parse[(A, B)] =
  parseA.flatMap(a =>
    parseB.map(b => (a, b))
    // parseB(str1) match
    //   case Some(b, str2) => Some((a, b), str2)
    //   case None => None
  )

def zip[A, B, C](parseA: Parse[A], parseB: Parse[B], parseC: Parse[C]): Parse[(A, B, C)] =
  zip(parseA, parseB).flatMap((a, b) =>
    parseC.map(c => (a, b, c))
  )

// Does not look like Scala has variadic templates like C++
def zip[A, B, C, D](
  parseA: Parse[A],
  parseB: Parse[B],
  parseC: Parse[C],
  parseD: Parse[D]): Parse[(A, B, C, D)] =
    zip(parseA, parseB, parseC).flatMap((a, b, c) =>
      parseD.map(d => (a, b, c, d))
    )

def zip[A, B, C, D, E](
  parseA: Parse[A],
  parseB: Parse[B],
  parseC: Parse[C],
  parseD: Parse[D],
  parseE: Parse[E]): Parse[(A, B, C, D, E)] =
    zip(parseA, parseB, parseC, parseD).flatMap((a, b, c, d) =>
      parseE.map(e => (a, b, c, d, e))
    )
