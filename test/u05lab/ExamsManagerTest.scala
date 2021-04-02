package u05lab

import org.junit.jupiter.api.Assertions.{assertEquals, assertFalse, assertTrue}
import org.junit.jupiter.api.Test
import u05lab.code.{ExamResult, ExamsManager, ExamsManagerImpl, Kind}
import scala.collection.Set

class ExamsManagerTest {

  val examsManager: ExamsManager = new ExamsManagerImpl

  @Test def testCreateFailedExam() {
    val exam: ExamResult = ExamResult.failed
    assertEquals(Kind.FAILED, exam.kind)
    assertTrue(exam.evaluation.isEmpty)
    assertFalse(exam.cumLaude)
  }

  @Test def testCreateRetiredExam() {
    val exam: ExamResult = ExamResult.retired
    assertEquals(Kind.RETIRED, exam.kind)
    assertTrue(exam.evaluation.isEmpty)
    assertFalse(exam.cumLaude)
  }

  @Test def testCreateSucceededCumLaudeExam() {
    val exam: ExamResult = ExamResult.succeededCumLaude
    assertEquals(Kind.SUCCEEDED, exam.kind)
    assertFalse(exam.evaluation.isEmpty)
    assertEquals(30, exam.evaluation.get)
    assertTrue(exam.cumLaude)
  }

  @Test def testCreateSucceededExam() {
    val evaluation: Int = 25
    val exam: ExamResult = ExamResult.succeeded(evaluation)
    assertEquals(Kind.SUCCEEDED, exam.kind)
    assertFalse(exam.evaluation.isEmpty)
    assertEquals(evaluation, exam.evaluation.get)
    assertFalse(exam.cumLaude)
  }

  @Test def testEvaluationWrongValue() {
    val evaluationTooLow: Int = 16
    try ExamResult.succeeded(evaluationTooLow) catch {case _: IllegalArgumentException => }
    val evaluationTooBig: Int = 32
    try ExamResult.succeeded(evaluationTooBig) catch {case _: IllegalArgumentException => }
  }

  def prepareExams() {
    examsManager.createNewCall("january")
    examsManager.createNewCall("february")
    examsManager.createNewCall("march")

    examsManager.addStudentResult("january", "rossi", ExamResult.failed)
    examsManager.addStudentResult("january", "bianchi", ExamResult.retired)
    examsManager.addStudentResult("january", "verdi", ExamResult.succeeded(28))
    examsManager.addStudentResult("january", "neri", ExamResult.succeededCumLaude)

    examsManager.addStudentResult("february", "rossi", ExamResult.failed)
    examsManager.addStudentResult("february", "bianchi", ExamResult.succeeded(20))
    examsManager.addStudentResult("february", "verdi", ExamResult.succeeded(30))

    examsManager.addStudentResult("march", "rossi", ExamResult.succeeded(25))
    examsManager.addStudentResult("march", "bianchi", ExamResult.succeeded(25))
    examsManager.addStudentResult("march", "viola", ExamResult.failed)
  }

  @Test def testExamsManagerGetStudentsByCall() {
    prepareExams()
    assertEquals(Set("rossi","bianchi", "verdi", "neri"), examsManager.allStudentFromCall("january"))
    assertEquals(Set("rossi", "bianchi", "verdi"), examsManager.allStudentFromCall("february"))
    assertEquals(Set("rossi", "bianchi", "viola"), examsManager.allStudentFromCall("march"))
  }

  @Test def testExamsManagerGetEvaluationFromCall() {
    prepareExams()
    assertEquals(2, examsManager.evaluationsMapFromCall("january").size)
    assertEquals(28, examsManager.evaluationsMapFromCall("january")("verdi"))
    assertEquals(30, examsManager.evaluationsMapFromCall("january")("neri"))

    assertEquals(2, examsManager.evaluationsMapFromCall("february").size)
    assertEquals(20, examsManager.evaluationsMapFromCall("february")("bianchi"))
    assertEquals(30, examsManager.evaluationsMapFromCall("february")("verdi"))

    assertEquals(2, examsManager.evaluationsMapFromCall("march").size)
    assertEquals(25, examsManager.evaluationsMapFromCall("march")("rossi"))
    assertEquals(25, examsManager.evaluationsMapFromCall("march")("bianchi"))
  }

  @Test def testResultsFromStudent() {
    prepareExams()

    assertEquals(3, examsManager.resultsMapFromStudent("rossi").size)
    assertEquals(Kind.FAILED.toString, examsManager.resultsMapFromStudent("rossi")("january"))
    assertEquals(Kind.FAILED.toString, examsManager.resultsMapFromStudent("rossi")("february"))
    assertEquals(Kind.SUCCEEDED.toString, examsManager.resultsMapFromStudent("rossi")("march"))

    assertEquals(2, examsManager.resultsMapFromStudent("verdi").size)
    assertEquals(Kind.SUCCEEDED.toString, examsManager.resultsMapFromStudent("verdi")("january"))
    assertEquals(Kind.SUCCEEDED.toString, examsManager.resultsMapFromStudent("verdi")("february"))

    assertEquals(3, examsManager.resultsMapFromStudent("bianchi").size)
    assertEquals(Kind.RETIRED.toString, examsManager.resultsMapFromStudent("bianchi")("january"))
  }

  @Test def testBestResultFromStudent(): Unit ={
    prepareExams()

    assertEquals(Option(25), examsManager.bestResultFromStudent("rossi"))
    assertEquals(Option(25), examsManager.bestResultFromStudent("bianchi"))
    assertEquals(Option(30), examsManager.bestResultFromStudent("neri"))
    assertEquals(Option.empty, examsManager.bestResultFromStudent("viola"))
  }

}
