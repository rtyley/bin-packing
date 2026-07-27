import ReleaseTransformations.*
import sbtversionpolicy.withsbtrelease.ReleaseVersion

organization := "com.madgag"
licenses := Seq(License.Apache2)

scalaVersion := "2.13.18"

scalacOptions := Seq("-deprecation", "-release:21")

Test / testOptions +=
  Tests.Argument(TestFrameworks.ScalaTest, "-u", s"test-results/scala-${scalaVersion.value}", "-o")

libraryDependencies ++= Seq(
  "com.madgag" %% "scala-collection-plus" % "1.0.0",
  "org.scalatest" %% "scalatest" % "3.2.20" % Test,
  "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test
)

releaseVersion := ReleaseVersion.fromAssessedCompatibilityWithLatestRelease().value
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  setNextVersion,
  commitNextVersion
)
