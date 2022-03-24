ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "maygen-scala"
  )

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.8.1" % "test",
  // https://mvnrepository.com/artifact/org.apache.commons/commons-lang3
  "org.apache.commons" % "commons-lang3" % "3.9",
  // https://mvnrepository.com/artifact/org.openscience.cdk/cdk-interfaces
  "org.openscience.cdk" % "cdk-interfaces" % "2.6",
  // https://mvnrepository.com/artifact/org.openscience.cdk/cdk-core
  //  "org.openscience.cdk" % "cdk-core" % "2.6",
  // https://mvnrepository.com/artifact/org.openscience.cdk/cdk-group
  "org.openscience.cdk" % "cdk-group" % "2.6",
  // https://mvnrepository.com/artifact/org.openscience.cdk/cdk-pdb
  "org.openscience.cdk" % "cdk-pdb" % "2.6",
  // https://mvnrepository.com/artifact/org.openscience.cdk/cdk-sdg
  "org.openscience.cdk" % "cdk-sdg" % "2.6",
  // https://mvnrepository.com/artifact/org.openscience.cdk/cdk-data
  "org.openscience.cdk" % "cdk-data" % "2.6",
  // https://mvnrepository.com/artifact/org.openscience.cdk/cdk-smiles
  "org.openscience.cdk" % "cdk-smiles" % "2.6",
)