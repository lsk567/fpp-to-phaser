name := "fpp-to-phaser"
ThisBuild / organization := "gov.nasa.jpl"
ThisBuild / scalaVersion := "3.1.2"

lazy val settings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xmax-inlines:100"
  ),
  libraryDependencies ++= dependencies, 
  Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oNCXELOPQRM"),
)

lazy val dependencies = Seq(
  "com.github.scopt" %% "scopt" % "4.0.1",
  "io.circe" %% "circe-core" % "0.14.3",
  "io.circe" %% "circe-generic" % "0.14.3",
  "io.circe" %% "circe-parser" % "0.14.3",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
  "org.scala-lang.modules" %% "scala-xml" % "2.1.0",
  "org.scalatest" %% "scalatest" % "3.2.12" % "test",
)

lazy val fpp_to_phaser = project
  .in(file("."))
  .settings(settings)
  .dependsOn(lib)
  .enablePlugins(AssemblyPlugin)

lazy val lib = (project in file("fpp/compiler/lib"))
  .settings(settings)

