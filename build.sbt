lazy val root = (project in file("."))
  .settings(
    scalaVersion := "2.13.2",
    name := "tapir-openapi-repro",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.client" %% "core" % "2.2.0",
      "com.softwaremill.sttp.client" %% "http4s-backend" % "2.2.0",
      "com.softwaremill.sttp.tapir" %% "tapir-core" % "0.16.1",
      "com.softwaremill.sttp.tapir" %% "tapir-sttp-client" % "0.16.1",
      "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % "0.16.1",
      "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % "0.16.1",
      "com.softwaremill.sttp.tapir" %% "tapir-openapi-docs" % "0.16.1",
      "com.softwaremill.sttp.tapir" %% "tapir-openapi-circe-yaml" % "0.16.1",
      "org.http4s" %% "http4s-blaze-server" % "0.21.6",
      "org.http4s" %% "http4s-blaze-client" % "0.21.6",
      compilerPlugin(
        "org.typelevel" % "kind-projector" % "0.11.0" cross CrossVersion.full
      )
    )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
