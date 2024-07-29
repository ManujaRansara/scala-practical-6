import scala.io.StdIn

object StudentRecords{
  def getStudentInfo():(String,Int,Int,Double,Char)={
    val (name, marks, totalMarks) = getStudentInfoWithRetry()
    val percentage=(marks.toDouble/totalMarks) * 100
    val grade=percentage match {
      case p if p >= 90 => 'A'
      case p if p >= 75 => 'B'
      case p if p >= 50 => 'C'
      case _ => 'D'
    }
    (name,marks,totalMarks,percentage,grade)
  }

  def printStudentRecord(student:(String,Int,Int,Double,Char)):Unit={
    val (name,marks,totalMarks,percentage,grade) =student
    println(s"Student Name:$name")
    println(s"Marks: $marks/$totalMarks")
    println(s"Percentage: $percentage%")
    println(s"Grade: $grade")
  }

  def validateInput(name:String,marks:String,totalMarks:String): (Boolean,Option[String])={
    if (name.trim.isEmpty){
      (false, Some("Name cannot be empty"))
    } else {
      try {
        val marksInt =marks.toInt
        val totalMarksInt =totalMarks.toInt
        if (marksInt < 0 || totalMarksInt < 0){
          (false,Some("Marks and total possible marks must be positive integers"))
        } else if (marksInt > totalMarksInt){
          (false,Some("Marks cannot exceed total possible marks"))
        } else {
          (true,None)
        }
      } catch {
        case _: NumberFormatException => (false, Some("Marks and total possible marks must be integers"))
      }
    }
  }

  def getStudentInfoWithRetry():(String,Int,Int)={
    var valid = false
    var name = ""
    var marks = 0
    var totalMarks = 0

    while (!valid) {
      println("Enter student name:")
      name = StdIn.readLine()
      println("Enter student marks:")
      val marksStr = StdIn.readLine()
      println("Enter total possible marks:")
      val totalMarksStr = StdIn.readLine()

      val (isValid, errorMessage) = validateInput(name, marksStr, totalMarksStr)
      if (isValid) {
        marks = marksStr.toInt
        totalMarks = totalMarksStr.toInt
        valid = true
      } else {
        println(s"Invalid input: ${errorMessage.get}")
      }
    }

    (name, marks, totalMarks)
  }

  def main(args:Array[String]):Unit ={
    val student =getStudentInfo()
    printStudentRecord(student)
  }
}


