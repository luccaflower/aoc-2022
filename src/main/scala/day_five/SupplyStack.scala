package supplies

import scala.io.Source
import scala.util.matching.Regex

object SupplyStack {
  def stacks(s: Seq[String]): Map[Int, List[Char]] = 
    s.map(_.grouped(4))
      .flatMap(_.map(_.charAt(1)).zipWithIndex.map{case (c,i) => (i+1 -> c)})
      .filter(_._2.isLetter)
      .groupMap(_._1)(_._2)
      .mapValues(_.toList)
      .toMap
  def digitsOf(s: String): Seq[Int] = """\d+""".r.findAllIn(s).map(_.toInt).toSeq
  def moves(s: Seq[String]): Seq[Move] = 
    s.map(digitsOf(_)).map(seq => Move(seq(0), seq(1), seq(2)))
  def parse(s: Seq[String]): SupplyStack = {
    val stackInput = s.takeWhile(_.strip.startsWith("["))
    val moveInput = s.reverse.takeWhile(_.startsWith("move"))
    SupplyStack(stacks(stackInput), moves(moveInput).toList.reverse)
  }
}
case class SupplyStack(val stacks: Map[Int, List[Char]], val moves: Seq[Move]) {
  def doMoves(): SupplyStack = {
    this.stacks.foreach(p => println(s"${p._1}: ${p._2.mkString}"))
    moves match {
      case (m :: rest) => {
        println(s"move ${m.amount} from ${m.src} to ${m.dst}")
        SupplyStack(
          move(m),
          rest
        ).doMoves()
      }
      case Nil => {
        this
      }
    }
  } 

  def move(m: Move): Map[Int, List[Char]] = 
    stacks
      .updated(m.dst, stacks(m.src).take(m.amount).reverse.:++(stacks(m.dst)))
      .updated(m.src, stacks(m.src).drop(m.amount))

  def message: String = stacks
    .toSeq.sortBy(_._1)
    .map(p => p._2(0)).mkString
}

case class Move(amount: Int, src: Int, dst: Int)

object MainPartOne {
  import supplies.SupplyStack._
  def main(args: Array[String]) = {
    val input = Source.fromFile(args(0)).getLines().toSeq
    println(parse(input).doMoves().message)
  }
}
