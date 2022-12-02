package rockpaperscissor
import scala.io.Source

sealed abstract class Shape(val score: Int, val letters: (String, String))

object Shape {
  def apply(s: String): Shape = 
    if (s.equals("A") || s.equals("X")) Rock
    else if (s == "B" || s == "Y") Paper
    else if (s == "C" || s == "Z") Scissors
    else throw new Exception

  case object Rock extends Shape(1, ("A", "X"))
  case object Paper extends Shape(2, ("B", "Y"))
  case object Scissors extends Shape(3, ("C", "Z"))
}

case class Match(val opp: Shape, val you: Shape) { self =>
  import Shape._
  val win = 6
  val draw = 3

  def score: Int = (self.opp, self.you) match {
    case (Rock, Paper) => win + Paper.score
    case (Paper, Scissors) => win + Scissors.score
    case (Scissors, Rock) => win + Rock.score
    case (_, _) if self.opp == self.you => draw + you.score
    case (_, _) => you.score
  }

}
object Match {
  def apply(s: String): Match = {
    val l = s.split(" ").toList
    val (x, y): (String, String) = l match {
      case List(a, b, _*) => (a, b)
    }
    Match(Shape.apply(x), Shape.apply(y))
  }

  def unapply(m: Match): (String, String) = 
    (m.opp.letters._1, m.you.letters._2)
}

object Parser {
  def parse(s: String): List[Match] = lines(s).map(l => Match.apply(l))
  def lines(s: String): List[String] = s.split("\n").toList
  def scores(m: List[Match]): List[Int] = m.map(_.score)
}



object MainPartOne {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile(args(0)).mkString
    val result = Parser.parse(input).map(_.score).sum
    println(result)
  }
}
