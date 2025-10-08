inThisBuild(
  List(
    organization := "tech.neander",
    homepage     := Some(url("https://github.com/neandertech/smithy-unison")),
    licenses     := List(License.Apache2),
    developers   := List(
      Developer(
        "Baccata",
        "Olivier Mélois",
        "baccata64@gmail.com",
        java.net.URI.create("https://github.com/baccata").toURL
      )
    ),
    version      := { if (sys.env.contains("CI")) version.value else "dev-SNAPSHOT" }
  )
)

scalaVersion := "3.7.1"

name         := "smithy-unison"
// it's a CLI plugin, not meant be used as a library.
crossVersion := CrossVersion.disabled

libraryDependencies ++= Seq(
  "software.amazon.smithy" % "smithy-build" % "1.61.0"
)
