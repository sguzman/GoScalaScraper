/** Name of project */
name := "GoScraper"

/** Organization */
organization := "com.github.sguzman"

/** Project Version */
version := "1.0"

/** Logging Level */
logLevel := util.Level.Debug

/** Scala version */
scalaVersion := "2.12.5"

/** Scalac parameters */
scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation", "-encoding", "utf8")

/** Javac parameters */
javacOptions ++= Seq("-server")

/** Resolver */
resolvers ++= Seq(
  DefaultMavenRepository,
  Resolver.sonatypeRepo("public"),
  Resolver.typesafeRepo("releases"),
  Resolver.sbtPluginRepo("releases"),
  Resolver.jcenterRepo,
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
)

/** Source Dependencies */
libraryDependencies ++= Seq(
  "org.scalaj" % "scalaj-http_2.12" % "2.3.0",
  "net.ruippeixotog" % "scala-scraper_2.12" % "2.1.0",
  "com.github.scopt" % "scopt_2.12" % "3.7.0"
)

/** Make sure to fork on run */
fork in run := true