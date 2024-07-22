@main def main3(): Unit = {
  def salary(h: Int, o: Int): Double = h * 250 + o * 85
  def getsalary(s:Double):Double = 0.88 *s*4
  println("The get salary is Rs :"+getsalary(salary(40,30)))
}