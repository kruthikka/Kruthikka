@main def main(): Unit = {
    def newlist(str_list: List[String]): List[String] = {
      val new_list = str_list.filter(x => x.length > 5)
      new_list
    }
    val strings = List("hello", "world", "Scala", "beautifull", "apple", "banana","pineapple")
    val result = newlist(strings)
    println(result)
  }
