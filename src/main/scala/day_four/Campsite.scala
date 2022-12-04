package campsite

import scala.io.Source
object Campsite {
  def sections(s: String): Seq[String] = s.split(',')
  def range(s: String): Range = {
    val range = s.split('-')
    (range(0).toInt to range(1).toInt)
  }
  def rangesOf(l: Seq[String]): (Range, Range) = 
    (range(l(0)), range(l(1)))
  def overlaps(p: (Range, Range)): Boolean =
    p._1.containsSlice(p._2) || p._2.containsSlice(p._1)

  def overlapsAtAll(p: (Range, Range)): Boolean =
    p._1.exists(p._2.contains(_))
}

object MainPartOne {
  import campsite.Campsite._
  def main(args: Array[String]) = {
    val input = Source.fromFile(args(0))
    val result = input.getLines()
      .map(sections(_))
      .map(rangesOf(_))
      .filter(overlaps(_))
      .length
    println(result)
  }
}

object MainPartTwo {
  import campsite.Campsite._
  def main(args: Array[String]) = {
    val input = Source.fromFile(args(0))
    val result = input.getLines()
      .map(sections(_))
      .map(rangesOf(_))
      .filter(overlapsAtAll(_))
      .length
    println(result)
  }
}
