package flower.lucca

import scala.jdk.StreamConverters._
import scala.io.Source

object Calories {
  def mostCaloriesOf: List[String] => Int = l => totalCaloriesOfEach(l).max
  

  def totalCaloriesOfEach(l: List[String]): List[Int] =
    l.foldLeft(List(0))((a, b) => b match {
      case s if s.isEmpty() => a.+:(0)
      case s => {
        val i = a(0)
        a.drop(1).+:(i + b.toInt)
      } 
    })
  
  def topThree(l: List[Int]): List[Int] = l.sortWith(_ > _).take(3)

  def parse(s: String): List[String] = s.lines().toScala(List).map(_.trim())
}

object MainPartOne {
  import Calories._
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile(args(0)).mkString
    println((parse _ andThen mostCaloriesOf)(lines))
  }
}

object MainPartTwo {
  import Calories._
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile(args(0)).mkString
    println((parse _ andThen totalCaloriesOfEach andThen topThree)(lines).sum)
  }

}


