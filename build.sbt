import sbt._
import Keys._
import sbtassembly.AssemblyPlugin.autoImport._

name := "SMC-Assembler"

version := "0.1"

scalaVersion := "2.12.3"

libraryDependencies += "org.rogach" %% "scallop" % "3.1.0"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

exportJars := true

lazy val commonSettings = Seq(
  version := "0.1-SNAPSHOT",
  organization := "com.github.chawasit",
  scalaVersion := "2.12.3",
  test in assembly := {}
)
