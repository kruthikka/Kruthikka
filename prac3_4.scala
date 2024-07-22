@main def main(): Unit = {
    val num_list = List(1, 2, 74, 45, 64, 12, 8, 48)
    
    def sum_even(list: List[Int]): Int = {
      val even_num = list.filter(x => x % 2 == 0)
      var sum = 0 
      for (num <- even_num) {
        sum = sum + num
      }
      sum
    }

    println("The sum of even numbers is " + sum_even(num_list))
  }