organization := "helicoidal"
name := "helicoidal:icfp-2024"
version := "0.1.0"
scalaVersion := "3.4.2"

// http://tpolecat.github.io/2017/04/25/scalac-flags.html
scalacOptions ++= Seq(
  "-deprecation",                  // Emit warning and location for usages of deprecated APIs.
  "-encoding", "utf-8",            // Specify character encoding used by source files.
  "-feature",                      // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials",        // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros", // Allow macro definition (besides implementation and application)
  "-language:higherKinds",         // Allow higher-kinded types
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-unchecked",                    // Enable additional warnings where generated code depends on assumptions.
  "-Xfatal-warnings",              // Fail the compilation if there are any warnings.
)

Compile / console / scalacOptions --= Seq(
  "-Ywarn-unused:imports",
  "-Xfatal-warnings",
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.12.0",
)

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "1.0.0" % Test
)

consoleQuick / initialCommands := """
  import cats._
  import cats.data._
  import cats.implicits._
"""

val root = Project("icfp-2024", file("."))
