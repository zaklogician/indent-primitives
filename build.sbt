lazy val root = (project in file(".")).
  settings(
    name := "indentor",
    version := "0.1.0",
    scalaVersion := "2.13.1",
    mainClass in Compile := Some("indentor.Main")
  )

// assemblyOutputPath in assembly := file("./deploy/scalaga" + ".jar")

libraryDependencies ++= Seq(
  //groupID % artifactID % revision % optionally test
  "org.scalacheck" %% "scalacheck" % "1.14.2" % "test"
)
