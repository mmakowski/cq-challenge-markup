import sbt._ 
 
class cq_challenge_markup(info: ProjectInfo) extends DefaultProject(info) { 
  val scalaToolsSnapshots = ScalaToolsSnapshots
  val scalatest = "org.scalatest" % "scalatest" % "1.2"
}
