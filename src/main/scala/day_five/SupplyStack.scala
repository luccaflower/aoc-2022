package supplies

import scala.io.Source
import scala.util.matching.Regex

object SupplyStack {
  type Stacks = Map[Int, List[Char]]
  def stacks(s: Seq[String]): Stacks =
    s.map(_.grouped(4))
      .flatMap(_.map(_.charAt(1)).zipWithIndex.map { case (c, i) =>
        (i + 1 -> c)
      })
      .filter(_._2.isLetter)
      .groupMap(_._1)(_._2)
      .mapValues(_.toList)
      .toMap
  def digitsOf(s: String): Seq[Int] =
    """\d+""".r.findAllIn(s).map(_.toInt).toSeq
  def moves(s: Seq[String]): Seq[Move] =
    s.map(digitsOf(_)).map(seq => Move(seq(0), seq(1), seq(2)))
  def parse(s: Seq[String]): SupplyStack = {
    val stackInput = s.takeWhile(_.strip.startsWith("["))
    val moveInput = s.reverse.takeWhile(_.startsWith("move"))
    SupplyStack(stacks(stackInput), moves(moveInput).toList.reverse)
  }

  def move(stacks: Stacks, m: Move): Stacks =
    stacks
      .updated(m.dst, stacks(m.src).take(m.amount).reverse.:++(stacks(m.dst)))
      .updated(m.src, stacks(m.src).drop(m.amount))
  def movePreserveOrder(stacks: Stacks, m: Move): Stacks =
    stacks
      .updated(m.dst, stacks(m.src).take(m.amount).:++(stacks(m.dst)))
      .updated(m.src, stacks(m.src).drop(m.amount))
}
case class SupplyStack(val stacks: Map[Int, List[Char]], val moves: Seq[Move]) {
  import SupplyStack._
  def doMoves(f: (Stacks, Move) => Stacks = move _): SupplyStack = {
    moves match {
      case (m :: rest) => {
        SupplyStack(
          f(stacks, m),
          rest
        ).doMoves(f)
      }
      case Nil => {
        this
      }
    }
  }

  def message: String = stacks.toSeq
    .sortBy(_._1)
    .map(p => p._2(0))
    .mkString
}

case class Move(amount: Int, src: Int, dst: Int)

object MainPartOne {
  import supplies.SupplyStack._
  def main(args: Array[String]) = {
    val input = Source.fromFile(args(0)).getLines().toSeq
    println(parse(input).doMoves().message)
  }
}

object MainPartTwo {
  import supplies.SupplyStack._
  def main(args: Array[String]) = {
    val input = Source.fromFile(args(0)).getLines().toSeq
    println(parse(input).doMoves(movePreserveOrder _).message)
  }
}
