import com.github.retronym.SbtOneJar._

name := "jwt"

version := "0.0.1"

scalaVersion := "2.10.0"

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

resolvers += "Sonatype OSS" at "http://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies ++= Seq(  "com.github.scopt" %% "scopt" % "3.2.0",
                              "org.bouncycastle" % "bcprov-ext-jdk15on" % "1.49",
                              "org.bouncycastle" % "bcpkix-jdk15on" % "1.49",
                              "com.google.guava" % "guava" % "15.0",
                              "com.google.code.findbugs" % "jsr305" % "2.+",
                              "io.argonaut" %% "argonaut" % "6.+",
                              "com.twitter" %% "finagle-http" % "6.+",
                              "com.netaporter" %% "scala-uri" % "0.4.0",
                              "org.scala-stm" %% "scala-stm" % "0.7" ) .map(_.excludeAll(
                              ExclusionRule(organization = "org.scala-tools.testing"),
                              ExclusionRule(organization = "junit"),
                              ExclusionRule(organization = "org.mockito"),
                              ExclusionRule(organization= "com.typesafe.sbt")
                              )
                              )

artifactName in oneJar := {
  (config: ScalaVersion, module: ModuleID, artifact: Artifact) =>
    artifact.name + "." + artifact.extension
}

// theon's URI implementation has a bug. Until that's fixed upstream, I need this to include the source directly here. That's easier than including a jar for distribution purposes
//libraryDependencies += "org.parboiled" %% "parboiled-scala" % "1.1.4"
