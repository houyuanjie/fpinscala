ThisBuild / scalaVersion := "3.0.1"

ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "icu.harx"
ThisBuild / organizationName := "harx"

lazy val commonDependencies =
  libraryDependencies ++= Seq("org.scalacheck" %% "scalacheck" % "1.15.4")

lazy val fpinscala = (project in file("."))
  .aggregate(`generic-structures`)
  .settings(name := "fpinscala", commonDependencies)

lazy val `generic-structures` = (project in file("generic-structures"))
  .settings(name := "generic-structures", commonDependencies)
