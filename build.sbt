ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file("."))
  .settings(
    name := "slidingblocks"
  )

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "0.7.26" % Test,
  "org.scalacheck" %% "scalacheck" % "1.15.4" % Test
)

testFrameworks += new TestFramework("munit.Framework")
