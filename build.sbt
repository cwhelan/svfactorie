import AssemblyKeys._

name := "SVfactorie"
 
version := "0.1"
 
scalaVersion := "2.10.2"

resolvers += "IESL Release" at "http://dev-iesl.cs.umass.edu/nexus/content/groups/public"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "cc.factorie" % "factorie" % "1.0.0-M7"

libraryDependencies += "org.rogach" %% "scallop" % "0.9.4"

assemblySettings
