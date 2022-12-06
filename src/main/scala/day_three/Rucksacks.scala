package rucksacks
import scala.io.Source
object Rucksacks {
  def halvesOf(s: String): (String, String) = s.splitAt(s.length / 2)
  def inBoth(c: (String, String)): Char = c._1.find(c._2.contains(_)).get
  def inEach(l: Seq[String]): Char =
    l(0).find(c => l(1).contains(c) && l(2).contains(c)).get
}

case class Item(c: Char) {
  def priority: Int =
    if (c.isUpper) c.toInt - 38
    else c.toInt - 96
}

object MainPartOne {
  import Rucksacks._
  def main(args: Array[String]) = {
    val input = Source.fromFile(args(0))
    val result = input
      .getLines()
      .map(halvesOf(_))
      .map(inBoth(_))
      .map(Item(_).priority)
      .sum
    println(result)
  }
}

object MainPartTwo {
  import Rucksacks._
  def main(args: Array[String]) = {
    val input = Source.fromFile(args(0))
    val result = input
      .getLines()
      .grouped(3)
      .map(inEach(_))
      .map(Item(_).priority)
      .sum
    println(result)
  }
}
