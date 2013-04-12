import sbt._
import Keys._
import org.scalatra.sbt._
import sbtassembly.Plugin.{MergeStrategy, assemblySettings}
import sbtassembly.Plugin.AssemblyKeys._
import com.mojolly.scalate.ScalatePlugin._
import ScalateKeys._

object GHTorrentNetViz extends Build {
  val Organization = "org.ghtorrent"
  val Name = "ghtorrent-net-viz"
  val Version = "0.1.0-SNAPSHOT"
  val ScalaVersion = "2.10.0"
  val ScalatraVersion = "2.2.0"

  lazy val project = Project (
    "ghtorrent-network-viz",
    file("."),
    settings = Defaults.defaultSettings ++ ScalatraPlugin.scalatraWithJRebel ++ scalateSettings ++
      assemblySettings ++ Seq(
      organization := Organization,
      name := Name,
      version := Version,
      scalaVersion := ScalaVersion,
      resolvers += Classpaths.typesafeReleases,
      libraryDependencies ++= Seq(
        "org.scalatra" %% "scalatra" % ScalatraVersion,
        "org.scalatra" %% "scalatra-scalate" % ScalatraVersion,
        "org.scalatra" %% "scalatra-specs2" % ScalatraVersion % "test",
        "net.sf.jung" % "jung-algorithms" % "2.0.1",
        "net.sf.jung" % "jung-graph-impl" % "2.0.1",
        "ch.qos.logback" % "logback-classic" % "1.0.6" % "runtime",
        "org.scalatra" %% "scalatra-json" % "2.2.0",
        "org.json4s"   %% "json4s-jackson" % "3.1.0",
        "org.eclipse.jetty" % "jetty-webapp" % "8.1.8.v20121106" % "compile;container",
        "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "compile;container;provided;test" artifacts (Artifact("javax.servlet", "jar", "jar"))
      ),
      scalateTemplateConfig in Compile <<= (sourceDirectory in Compile){ base =>
        Seq(
          TemplateConfig(
            base / "webapp" / "WEB-INF" / "templates",
            Seq.empty,  /* default imports should be added here */
            Seq.empty,  /* add extra bindings here */
            Some("templates")
          )
        )
      },
      test in assembly := {},
      mergeStrategy in assembly := { x =>
        x.toLowerCase match {
        case "meta-inf/manifest.mf" => MergeStrategy.discard
        case _ => MergeStrategy.last
      }
      }
    )
  )
}
