package rockpaperscissor
import scala.io.Source

sealed abstract class Shape(val score: Int) {
  def losesTo: Shape
  def drawsWith: Shape
  def winsAgainst: Shape
}

object Shape {
  def apply(s: String): Shape = 
    if (s.equals("A") || s.equals("X")) Rock
    else if (s == "B" || s == "Y") Paper
    else if (s == "C" || s == "Z") Scissors
    else throw new Exception

  case object Rock extends Shape(1) {
    override def drawsWith: Shape = Rock
    override def winsAgainst: Shape = Scissors
    override def losesTo: Shape = Paper
  } 
  case object Paper extends Shape(2) {
    override def drawsWith: Shape = Paper
    override def winsAgainst: Shape = Rock
    override def losesTo: Shape = Scissors
  }
  case object Scissors extends Shape(3) {
    override def drawsWith: Shape = Scissors
    override def winsAgainst: Shape = Paper
    override def losesTo: Shape = Rock
  }
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
  def apply(s: String)(decider: (Shape, String) => Shape = byLetter): Match = {
    val l = s.split(" ").toList
    val (x, y): (String, String) = l match {
      case List(a, b, _*) => (a, b)
    }
    val opp = Shape.apply(x)
    Match(opp, decider(opp, y))
  }

  def byLetter: (Shape, String) => Shape = (_, s) => Shape.apply(s)
  def byOpponent: (Shape, String) => Shape = (opp, s) => s match {
    case "X" => opp.winsAgainst
    case "Y" => opp.drawsWith
    case "Z" => opp.losesTo
  }

}

object Parser {
  def matches(l: List[String]): List[Match] = l.map(l => Match.apply(l)())
  def lines(s: String): List[String] = s.split("\n").toList
  def scores(m: List[Match]): List[Int] = m.map(_.score)
}



object MainPartOne {
  import Parser._
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile(args(0)).mkString
    val result = (lines _ andThen matches andThen scores)(input).sum
    println(result)
  }
}

object MainPartTwo {
  import  Parser._
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile(args(0)).mkString
    val result = lines(input).map(Match.apply(_)(Match.byOpponent).score).sum
    println(result)
  }
}
