ThisBuild / version := "2022.1.1"
ThisBuild / scalaVersion := "2.13.10"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
lazy val root = (project in file("."))
  .settings(
    name := "sudokuSolver",
    idePackagePrefix := Some("dev.cjens")
  )
