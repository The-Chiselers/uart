// See README.md for license details.

ThisBuild / scalaVersion := "2.13.15"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "tech.rocksavage"
ThisBuild / organizationName := "Rocksavage Technology"

Test / parallelExecution := false

lazy val chisel_module_runner = RootProject(
  uri("https://github.com/The-Chiselers/chisel_module_runner.git#main")
)
lazy val stdlib = RootProject(
  uri("https://github.com/The-Chiselers/stdlib.git#main")
)
lazy val synth = RootProject(
  uri("https://github.com/The-Chiselers/synth.git#main")
)
lazy val addrdecode = RootProject(
  uri("https://github.com/The-Chiselers/addrdecode.git#main")
)
lazy val apb = RootProject(uri("https://github.com/The-Chiselers/apb.git#main"))
lazy val registermap = RootProject(
  uri("https://github.com/The-Chiselers/registermap.git#main")
)
lazy val dynamicfifo_one_cycle = RootProject(
  uri("https://github.com/The-Chiselers/dynamicfifo_one_cycle.git#main")
)
lazy val root = (project in file("."))
  .settings(
    name := "uart",
    Test / publishArtifact := true,
    libraryDependencies ++= Seq(
      "org.chipsalliance" %% "chisel" % chiselVersion,
      "edu.berkeley.cs" %% "chiseltest" % "6.0.0",
      "org.rogach" %% "scallop" % "5.2.0"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-Ymacro-annotations"
    ),
    addCompilerPlugin(
      "org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full
    ),
    // (make sure you ran `sbt publishLocal` in chiselware_lints first)
    scalafixDependencies += "tech.rocksavage" %% "chiselware_lints" % "0.1.0",
    // === NEW: run scalafix on every compilation ===
    scalafixOnCompile := true
  )
  .dependsOn(
    chisel_module_runner,
    stdlib,
    synth,
    addrdecode,
    apb,
    registermap,
    dynamicfifo_one_cycle
  )
val chiselVersion = "6.6.0"
val scalafmtVersion = "2.5.0"

// Scala coverage settings
coverageDataDir := target.value / "../generated/scalaCoverage"
coverageFailOnMinimum := true
coverageMinimumStmtTotal := 90
coverageMinimumBranchTotal := 95
