
lazy val attoVersion                 = "0.8.0"
lazy val catsVersion                 = "2.1.1"
lazy val collCompatVersion           = "2.1.6"
lazy val jtsVersion                  = "1.16.1"
lazy val kindProjectorVersion        = "0.10.3"
lazy val monocleVersion              = "2.0.4"
lazy val catsTestkitScalaTestVersion = "1.0.1"
lazy val scalaJavaTimeVersion        = "2.0.0"

inThisBuild(Seq(
  homepage := Some(url("https://github.com/gemini-hlsw/gsp-math")),
  addCompilerPlugin("org.typelevel" %% "kind-projector" % kindProjectorVersion),
  Global / onChangedBuildSource := ReloadOnSourceChanges
) ++ gspPublishSettings)

skip in publish := true


lazy val jsts = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/jsts"))
  .settings(
    name := "gpp-jsts",
    publishArtifact in (Compile, packageDoc) := false,
    scalacOptions ~= (_.filterNot(
      Set(
        // By necessity facades will have unused params
        "-Wdead-code",
        "-Wunused:params",
        "-Ywarn-dead-code",
        "-Ywarn-unused:params"
      )))
  )
  .jvmConfigure(_.enablePlugins(AutomateHeaderPlugin))
  .jvmSettings(
//    libraryDependencies ++= Seq(
//      "org.locationtech.jts" % "jts-core" % jtsVersion
//    )
  )
  .jsSettings(gspScalaJsSettings: _*)
  .jsSettings(
    //libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % scalaJavaTimeVersion,
     scalaJSUseMainModuleInitializer := true,
  )

