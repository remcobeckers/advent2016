lazy val advent2016 = project
  .in(file("."))
  .enablePlugins(GitVersioning)

libraryDependencies ++= Vector(
  Library.scalaTest % "test"
)
