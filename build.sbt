ThisBuild / scalaVersion := "3.6.4"
ThisBuild / version := "0.0.1"
ThisBuild / javacOptions ++= Seq(
  "--enable-preview",
  "--release", "23"
)

lazy val popcorn = (project in file("."))
  .settings(
    name := "popcorn",
    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % "33.4.0-jre",
      "net.datafaker" % "datafaker" % "2.4.2",
    )
  )