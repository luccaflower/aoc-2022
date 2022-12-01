import org.scalatest.funsuite._
import org.scalatest.matchers.should.Matchers
import flower.lucca.Calories

class CaloriesParserTest extends AnyFunSuite with Matchers {
  test("a number is an amount of calories") {
    Calories.mostCaloriesOf(List("1000")) should equal(1000)
  }
  
  test("calories on a single elf are summed up") {
    Calories.mostCaloriesOf(List("1000", "2000")) should equal(3000)
  }

  test("empty calories denotes new elf") {
    Calories.mostCaloriesOf(List("1000", "2000", "", "2000")) should equal(3000)
  }

  test("empty lines denote empty calories") {
    val lines = """
      1000
      2000

      4000
      """
    Calories.mostCaloriesOf(
      Calories.parse(lines)) should equal(4000)
  }
}
