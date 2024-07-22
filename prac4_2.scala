import scala.io.StdIn

@main def main(): Unit = {
    print("Enter an integer: ")
    var integer: Int = StdIn.readInt()
    
    integer match {
      case n if n <= 0 => println("Negative/Zero is input")
      case n if n % 2 == 0 => println("Even number is given")
      case n => println("Odd number is given")
    }
}