import org.scalatest.funsuite._
import org.scalatest.matchers.should._
import scala.collection.immutable.HashMap
import supplies.SupplyStack

class SupplyStacksTest extends AnyFunSuite with Matchers {
  val input =
    """    [D]    
      |[N] [C]    
      |[Z] [M] [P]
      | 1   2   3 
      |
      |move 1 from 2 to 1
      |move 3 from 1 to 3
      |move 2 from 2 to 1
      |move 1 from 1 to 2""".stripMargin
  import supplies.SupplyStack._
  import supplies.Move

  test("a stack is a map of towers") {
    stacks(List(input.linesIterator.toList(0))) should equal(
      HashMap(2 -> Seq('D'))
    )
  }

  test("a move is 3 numbers") {
    val digits = moves(List("move 1 from 2 to 1"))(0) should be(Move(1, 2, 1))
  }

  test("initial state") {
    parse(input.linesIterator.toSeq).stacks should equal(
      Map(1 -> List('N', 'Z'), 2 -> List('D', 'C', 'M'), 3 -> List('P'))
    )
  }

  test("do a move") {
    val stack = parse(input.linesIterator.toSeq)
    stack.moves(0) should be(Move(1, 2, 1))
    move(stack.stacks, stack.moves(0)) should equal(
      Map(1 -> List('D', 'N', 'Z'), 2 -> List('C', 'M'), 3 -> List('P'))
    )
  }

  test("do the moves") {
    parse(input.linesIterator.toSeq).doMoves().stacks should equal(
      Map(1 -> List('C'), 2 -> List('M'), 3 -> List('Z', 'N', 'D', 'P'))
    )
  }

  test("do the moves and preserve order") {
    parse(input.linesIterator.toSeq)
      .doMoves(movePreserveOrder _)
      .message should equal("MCD")
  }

  test("get result") {
    parse(input.linesIterator.toSeq)
      .doMoves()
      .stacks
      .map(p => p._2(0))
      .mkString should equal("CMZ")
  }
}
