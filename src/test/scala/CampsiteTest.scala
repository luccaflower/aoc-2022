import org.scalatest.funsuite._
import org.scalatest.matchers.should._

class CampsiteTest extends AnyFunSuite with Matchers {
  val input =
    """2-4,6-8
      |2-3,4-5
      |5-7,7-9
      |2-8,3-7
      |6-6,4-6
      |2-6,4-8""".stripMargin
  import campsite.Campsite._
  test("a line is an assignment") {
    input.linesIterator
      .map(sections(_))
      .toList(0)(0) should equal("2-4")
  }

  test("an assignment is a range of sections") {
    input.linesIterator
      .map(sections(_))
      .map(rangesOf(_))
      .toList(0)
      ._1 should equal(2 to 4)
  }

  test("in an assignment pair, one might contain the other") {
    input.linesIterator
      .map(sections(_))
      .map(rangesOf(_))
      .filter(overlaps(_))
      .length should equal(2)
  }

  test("an assignment pair may have any overlap at all") {
    input.linesIterator
      .map(sections(_))
      .map(rangesOf(_))
      .filter(overlapsAtAll(_))
      .length should equal(4)
  }
}
