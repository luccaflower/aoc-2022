package flower.lucca

import scala.jdk.StreamConverters._
import scala.io.Source

object Calories {
  def mostCaloriesOf(l: List[String]): Int = {
    l.foldLeft(List(0))((a, b) => b match {
      case s if s.isEmpty() => a.+:(0)
      case s => {
        val i = a(0)
        a.drop(1).+:(i + b.toInt)
      } 
    }).max
  }

  def parse(s: String): List[String] = s.lines().toScala(List).map(_.trim())
  def main(args: Array[String]): Unit = 
    println(mostCaloriesOf(parse(Source.fromFile(args(0)).mkString)))
}
