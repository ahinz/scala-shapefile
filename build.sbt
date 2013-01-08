
name := "Scala Shapefile Parser"

scalaVersion := "2.10.0"

libraryDependencies += "com.vividsolutions" % "jts" % "1.13"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M5b" % "test"

resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
