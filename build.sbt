lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "br.ufma.ecp",
      scalaVersion := "3.5.2"
    )),
    name := "slox"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test
