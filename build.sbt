name := "snippets"
scalaVersion := "2.13.3"
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.3.0",
  "org.typelevel" %% "cats-laws" % "2.3.0" % Test,
  "org.typelevel" %% "discipline-munit" % "1.0.3" % Test
)

testFrameworks += new TestFramework("munit.Framework")
scalacOptions ++= Seq(
  "-deprecation"
)
