ThisBuild / scalaVersion := "3.6.4"
ThisBuild / version := "0.0.1"
ThisBuild / fork := true
ThisBuild / javacOptions ++= Seq(
  "--enable-preview",
  "--release", "23",
  "--add-modules", "jdk.incubator.vector"
)

ThisBuild / scalacOptions ++= Seq(
  "-deprecation"
)

lazy val popcorn = (project in file("."))
  .enablePlugins(Antlr4Plugin)
  .settings(
    name := "popcorn",
    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % "33.4.5-jre",
      "net.datafaker" % "datafaker" % "2.4.2",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
      "ch.qos.logback" % "logback-classic" % "1.5.18",
      "org.scalactic" %% "scalactic" % "3.2.19",
      "org.scalatest" %% "scalatest" % "3.2.19" % Test
    )
  )
  .settings(
    Antlr4 / antlr4Version := "4.13.2",
    Antlr4 / antlr4GenListener := false,
    Antlr4 / antlr4GenVisitor := false,
    Antlr4 / antlr4PackageName := Some("io.exsql.popcorn.sexpr.antlr4")
  )