import org.scalatest.funsuite._
import org.scalatest.matchers.should._

class SignalTest extends AnyFunSuite with Matchers {
  val input = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

  import signal.Signal._
  test("characters after first marker") {
    process(input) should be(7)
  }

  test("now with custom length") {
    process(input, 14) should be(19)
  }
}
