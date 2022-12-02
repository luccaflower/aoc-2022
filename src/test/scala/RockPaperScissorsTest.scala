import org.scalatest.funsuite._
import org.scalatest.matchers.should._

class RockPaperScissorsTest extends AnyFunSuite with Matchers {
  import rockpaperscissor._
  test("a line is a match") {
    val m = Match.apply("A X")
    m.opp should be(Shape.Rock)
    m.you should be(Shape.Rock)
  }
    
  test("A is Rock") {
    Shape.apply("A") should be(Shape.Rock)
  }
  
  test("same shape is a draw") {
    Match(Shape.Rock, Shape.Rock).score should equal(4)
  }

  test("a single line is a single match") {
    Parser.lines("A X").size should equal(1)
  }
}
