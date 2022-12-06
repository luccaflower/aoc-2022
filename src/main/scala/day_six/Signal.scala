package signal

import scala.annotation.tailrec
import scala.io.Source
object Signal {
  def process(s: String, l: Int = 4): Int = {
    @tailrec
    def go(s: String, acc: Int): Int =
      if (s.take(l).distinct.length == l) acc
      else go(s.drop(1), acc+1)
    go(s, l)    
  }
}

object MainPartOne {
  import Signal._
  def main(args: Array[String]) = {
    val input = Source.fromFile(args(0)).mkString
    println(process(input))
  }
}

object MainPartTwo {
  import Signal._
  def main(args: Array[String]) = {
    val input = Source.fromFile(args(0)).mkString
    println(process(input, 14))
  }
}
