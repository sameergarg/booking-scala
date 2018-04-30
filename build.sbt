name := "booking-scala"

version := "0.1"

scalaVersion := "2.12.5"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.1.0",
  "org.typelevel" %% "cats-effect" % "1.0.0-RC"
)

scalacOptions += "-Ypartial-unification"