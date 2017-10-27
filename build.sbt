name := "fps-workshop"

version := "1.0"

scalaVersion := "2.12.3"

libraryDependencies ++= Seq(
  "com.lihaoyi" % "ammonite" % "1.0.2" % "test" cross CrossVersion.full
)

val predef = Seq(
  "import ch6._",
  "import utils._"
).mkString(";")

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, s"""object amm extends App { ammonite.Main("$predef").run() }""")
  Seq(file)
}.taskValue
