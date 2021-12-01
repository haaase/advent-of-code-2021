lazy val root = (project in file("."))
  .settings(
    name := "aoc-21",
    scalaVersion := "3.1.0",
    libraryDependencies += "org.typelevel" %% "cats-core"         % "2.6.1",
    libraryDependencies += "org.typelevel" %% "cats-effect"       % "3.2.9",
    libraryDependencies += "com.monovore"  %% "decline"           % "2.2.0",
  )
  .settings(
    // native-image flag "--initialize-at-build-time" is required for Cats Effect applications
    nativeImageOptions ++= List("--initialize-at-build-time", "--no-fallback",
    // "--report-unsupported-elements-at-runtime", // makes application crash in certain cases
    ))
  .enablePlugins(NativeImagePlugin)
  .settings(Compile / mainClass := Some("CMD"))
//   .dependsOn(scala_2_13)