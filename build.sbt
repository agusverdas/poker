name := "poker"

version := "0.1"

scalaVersion := "2.13.5"

idePackagePrefix := Some("edu.agus.poker")

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-Ymacro-annotations",
)

val scalaTestVersion = "3.1.0.0-RC2"
val catsScalacheckVersion = "0.2.0"
val catsVersion = "2.2.0"
val circeVersion = "0.13.0"
val catsEffectVersion = "2.2.0"
val http4sVersion = "0.21.22"
val doobieVersion = "0.9.0"
val enumeratumVersion = "1.6.1"
val akkaVersion = "2.6.9"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectVersion,
  "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion % Test,
  "org.scalatestplus" %% "selenium-2-45" % scalaTestVersion % Test,
  "io.chrisdavenport" %% "cats-scalacheck" % catsScalacheckVersion % Test,
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "org.http4s" %% "http4s-jdk-http-client" % "0.3.6",
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-generic-extras" % circeVersion,
  "io.circe" %% "circe-optics" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.tpolecat" %% "doobie-core" % doobieVersion,
  "org.tpolecat" %% "doobie-h2" % doobieVersion,
  "org.tpolecat" %% "doobie-hikari" % doobieVersion,
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.beachape" %% "enumeratum" % enumeratumVersion,
  "com.beachape" %% "enumeratum-doobie" % "1.6.0",
  "com.beachape" %% "enumeratum-circe" % enumeratumVersion,
  "com.h2database" % "h2" % "1.4.200",
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test,
  "org.scalaj" %% "scalaj-http" % "2.4.2" % Test
)
