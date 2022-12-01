lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "flower.lucca",
      scalaVersion := "2.13.6"
    )),
    name := "advent-of-code-2022"
  )

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.9" % Test,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"
)
