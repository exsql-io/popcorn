ThisBuild / scalaVersion := "3.6.4"
ThisBuild / version := "0.0.1"

lazy val popcorn = (project in file("."))
  .settings(
    name := "popcorn",
    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % "33.4.0-jre"
    )
  )