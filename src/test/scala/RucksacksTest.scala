import org.scalatest.funsuite._
import org.scalatest.matchers.should._

class RucksacksTest extends AnyFunSuite with Matchers {
  val input =
    """vJrwpWtwJgWrhcsFMMfFFhFp
      |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
      |PmmdzqPrVvPwwTWBwg
      |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
      |ttgJtRGJQctTZtZT
      |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin
  import rucksacks.Rucksacks._
  import rucksacks.Item
  test("split at length/2 splits in half") {
    val firstLine = input.linesIterator
      .map(halvesOf(_))
      .toList(0)
    firstLine._1.length should equal(firstLine._2.length)
  }

  test("items have priority") {
    Item('a').priority should equal(1)
    Item('z').priority should equal(26)
    Item('A').priority should equal(27)
    Item('Z').priority should equal(52)
  }

  test("for each rucksack, one item appears in both halves") {
    input.linesIterator
      .map(halvesOf(_))
      .map(inBoth(_))
      .toList(0) should equal('p')
  }

  test(
    "a rucksack has the priority of the item that appears in both compartments"
  ) {
    input.linesIterator
      .map(halvesOf(_))
      .map(inBoth(_))
      .map(Item(_).priority)
      .sum should equal(157)
  }

  test("for each three rucksacks, and item appears in each") {
    input.linesIterator
      .grouped(3)
      .map(inEach(_))
      .map(Item(_).priority)
      .sum should equal(70)
  }
}
