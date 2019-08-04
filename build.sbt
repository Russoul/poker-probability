name := "poker-calc"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0"   //functional
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.3.1"
libraryDependencies += "org.fusesource.jansi" % "jansi" % "1.18"  //cross-platform formatting in terminal

libraryDependencies ++= Seq(
  "com.github.julien-truffaut" %%  "monocle-core"  % "1.6.0",
  "com.github.julien-truffaut" %%  "monocle-macro" % "1.6.0",
  "com.github.julien-truffaut" %%  "monocle-law"   % "1.6.0" % "test"
)

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)