package u05lab.code

import scala.collection.{Set, mutable}

//enum for kind
sealed trait Kind
//Kind companion object
object Kind {
  case object RETIRED extends Kind
  case object FAILED extends Kind
  case object SUCCEEDED extends Kind
}

sealed trait ExamResult{
  def kind: Kind
  def evaluation: Option[Int]
  def cumLaude: Boolean
}

object ExamResult {
  def failed: ExamResult = ExamResultImpl(Kind.FAILED, Option.empty, cumLaude = false)
  def retired: ExamResult = ExamResultImpl(Kind.RETIRED, Option.empty, cumLaude = false)
  def succeededCumLaude: ExamResult = ExamResultImpl(Kind.SUCCEEDED, evaluation = Option(30), cumLaude = true)
  def succeeded(evaluation: Int): ExamResult =
    if (evaluation >= 18 && evaluation <= 30)
      ExamResultImpl(Kind.SUCCEEDED, evaluation = Option(evaluation), cumLaude = false)
    else
      throw new IllegalArgumentException

  private case class ExamResultImpl(kind: Kind, evaluation: Option[Int], cumLaude: Boolean) extends ExamResult
}

trait ExamsManager{
  def createNewCall(call: String)
  def addStudentResult(call: String, student: String, result: ExamResult)
  def allStudentFromCall(call: String): Option[Set[String]]
  def evaluationsMapFromCall(call: String): Option[Map[String, Int]]
  def resultsMapFromStudent(student: String): Map[String, String]
  def bestResultFromStudent(student: String): Option[Int]
}

class ExamsManagerImpl extends ExamsManager {
  var callsAndResultsMap: Map[String, Map[String, ExamResult]] = Map()

  override def createNewCall(call: String): Unit = callsAndResultsMap += (call -> Map())

  override def addStudentResult(call: String, student: String, result: ExamResult){
    val updateEvaluations = callsAndResultsMap(call) + (student -> result)
    callsAndResultsMap += (call -> updateEvaluations)
  }

  override def allStudentFromCall(call: String): Option[Set[String]] = callsAndResultsMap.get(call) match {
    case Some(m) => Option(m.keySet)
    case _  => Option.empty
  }

  override def evaluationsMapFromCall(call: String): Option[Map[String, Int]] = callsAndResultsMap.get(call) match {
    case Some(m) => Option(m collect {
      case (name, result) if result.kind.equals(Kind.SUCCEEDED) => name -> result.evaluation.get
    })
    case _ => Option.empty
  }

  override def resultsMapFromStudent(student: String): Map[String, String] = examsFromStudent(student) map {
    case (call, result) => call -> result.kind.toString
  }

  override def bestResultFromStudent(student: String): Option[Int] = examsFromStudent(student).values.map(res => res.evaluation).max

  private def examsFromStudent(student: String): Map[String, ExamResult] = callsAndResultsMap collect {
    case (call, map) if map.contains(student) => call -> map(student)
  }
}
