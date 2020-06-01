
inThisBuild(Seq(
  homepage := Some(url("https://github.com/gemini-hlsw/gpp-jts")),
  Global / onChangedBuildSource := ReloadOnSourceChanges
) ++ gspPublishSettings)

skip in publish := true

lazy val jts = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/jts"))
  .settings(
    name := "gpp-jts",
    // publishArtifact in (Compile, packageDoc) := false,
    scalacOptions in (Compile, packageDoc) ~= (_.filterNot(
      Set(
        "-Werror",
        "-Xlint:doc-detached",
        "-Xfatal-warnings"
      )
    )),
    scalacOptions ~= (_.filterNot(
      Set(
        // By necessity facades will have unused params
        "-Wdead-code",
        "-Wunused:params",
        "-Ywarn-dead-code",
        "-Ywarn-unused:params",
        "-Xlint:doc-detached"
      )))
  )

lazy val jts_awt = project
  .in(file("modules/jts-awt"))
  .settings(
    name := "gpp-jts-awt",
    // publishArtifact in (Compile, packageDoc) := false,
    scalacOptions in (Compile, packageDoc) ~= (_.filterNot(
      Set(
        "-Werror"
      )
    )),
    scalacOptions ~= (_.filterNot(
      Set(
        // By necessity facades will have unused params
        "-Wdead-code",
        "-Wunused:params",
        "-Ywarn-dead-code",
        "-Ywarn-unused:params",
        "-Xlint:doc-detached"
      )))
  )
  .dependsOn(jts.jvm)

lazy val tests = project
  .in(file("modules/tests"))
  .settings(
    name := "jts-tests",
    skip in publish := true,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
  .dependsOn(jts.jvm)
