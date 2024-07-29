import scala.io.StdIn

object StudentRecordsApp extends App {
  
  // Function to validate user input
  def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
    if (name.isEmpty) {
      (false, Some("Name cannot be empty."))
    } else if (marks < 0 || marks > totalMarks) {
      (false, Some(s"Marks should be between 0 and $totalMarks."))
    } else {
      (true, None)
    }
  }
  
  // Function to read student info and calculate percentage and grade
  def getStudentInfo(): (String, Int, Int, Double, Char) = {
    println("Enter student name:")
    val name = StdIn.readLine()
    
    println("Enter marks obtained:")
    val marks = StdIn.readInt()
    
    println("Enter total possible marks:")
    val totalMarks = StdIn.readInt()
    
    val percentage = (marks.toDouble / totalMarks) * 100
    val grade = percentage match {
      case p if p >= 90 => 'A'
      case p if p >= 75 => 'B'
      case p if p >= 50 => 'C'
      case _ => 'D'
    }
    
    (name, marks, totalMarks, percentage, grade)
  }
  
  // Function to print student record
  def printStudentRecord(student: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = student
    println(s"Student Name: $name")
    println(s"Marks Obtained: $marks / $totalMarks")
    println(f"Percentage: $percentage%.2f%%")
    println(s"Grade: $grade")
  }
  
  // Function to get student info with retries until valid input is provided
  def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
    var isValid = false
    var student: (String, Int, Int, Double, Char) = ("", 0, 0, 0.0, 'D')
    
    while (!isValid) {
      val info = getStudentInfo()
      val (name, marks, totalMarks, _, _) = info
      val (valid, errorMessage) = validateInput(name, marks, totalMarks)
      
      if (valid) {
        isValid = true
        student = info
      } else {
        println(errorMessage.getOrElse("Invalid input. Please try again."))
      }
    }
    
    student
  }
  
  // Get student info with retries and print the student record
  val studentRecord = getStudentInfoWithRetry()
  printStudentRecord(studentRecord)
}
