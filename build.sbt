name := "DxFiler"
version := "0.0.2"

scalaVersion := "3.3.0"

crossPaths := false

scalacOptions ++= Seq("-encoding", "UTF-8")
scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
)

autoScalaLibrary := true

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
libraryDependencies += "net.lingala.zip4j" % "zip4j" % "2.11.5"
libraryDependencies += "org.json" % "json" % "20230227"

mainClass := Some("ice.Filer")

assembly / test := {}
assembly / mainClass := Some("ice.DxFiler")
assembly / assemblyJarName := "DxFiler.jar"

ThisBuild / assemblyMergeStrategy := {
  case PathList(x @ _*) if x.last.endsWith("module-info.class") => MergeStrategy.discard
  case x => (ThisBuild / assemblyMergeStrategy).value.apply(x)
}
