name := "delegert"
version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.11"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++=
  "org.scala-lang" % "scala-reflect" % scalaVersion.value ::
  "com.github.cornerman" %% "macroni" % "0.1.0-SNAPSHOT" % "test" ::
  "org.specs2" %% "specs2-core" % "3.8.4" % "test" ::
  "org.specs2" %% "specs2-mock" % "3.8.4" % "test" ::
  Nil

scalacOptions ++=
  "-encoding" :: "UTF-8" ::
  "-unchecked" ::
  "-deprecation" ::
  "-explaintypes" ::
  "-feature" ::
  "-language:_" ::
  "-Xlint:_" ::
  "-Ywarn-unused" ::
  Nil
