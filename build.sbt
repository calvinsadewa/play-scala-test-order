name := "play-scala-anorm-example"

version := "2.6.0-SNAPSHOT"

scalaVersion := "2.12.2"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

libraryDependencies += guice
libraryDependencies += jdbc
libraryDependencies += evolutions

libraryDependencies += "com.h2database" % "h2" % "1.4.194"

libraryDependencies += "com.typesafe.play" %% "anorm" % "2.5.3"
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.0" % Test
libraryDependencies += "org.mockito" % "mockito-all" % "1.9.5" % "test"
libraryDependencies += "com.typesafe.play" % "play-json-joda_2.12" % "2.6.3"