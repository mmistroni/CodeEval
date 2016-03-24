// Set the project name to the string 'My Project'
name := "CodeEval"

// The := method used in Name and Version is one of two fundamental methods.
// The other method is <<=
// All other initialization methods are implemented in terms of these.
version := "1.0"
scalaVersion := "2.11.7"

// Add a single dependency
libraryDependencies += "junit" % "junit" % "4.8" % "test"
libraryDependencies += "com.netflix.rxjava" % "rxjava-scala" % "0.19.1"
libraryDependencies += "org.scala-lang" % "scala-swing" % "2.11+"
libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5"
libraryDependencies += "org.scalamock" % "scalamock-core_2.11" % "3.2.2" % "test"
libraryDependencies += "org.scalamock" % "scalamock-scalatest-support_2.11" % "3.2.2" % "test"
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
libraryDependencies += "org.mockito" % "mockito-core" % "1.9.5"
libraryDependencies ++= Seq("org.slf4j" % "slf4j-api" % "1.7.5",
                            "org.slf4j" % "slf4j-simple" % "1.7.5",
                            "org.clapper" %% "grizzled-slf4j" % "1.0.2")