lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      name := "WaterIsWet",
      scalaVersion := "2.12.3",
      version      := "0.1.2"
    )),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.17"
  )
