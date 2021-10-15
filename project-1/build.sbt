lazy val root = (project in file("."))
  .settings(
    name := "CSE 250",
    scalaVersion := "2.13.6"
  )

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"
