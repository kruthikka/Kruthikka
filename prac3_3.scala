@main def main(): Unit = {
    def mean(x: Int, y: Int): Float = {
      val sum = x + y
      val average = sum.toFloat / 2
      Math.round(average * 100) / 100.0f
    }
    println("The mean is " + mean(14, 13))
  }