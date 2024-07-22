@main def main2(): Unit = {
  var a=2
  var b = 3
  var c = 4
  var d = 5
  var k = 4.3f

  b=b-1 
  val one= b*a+c*d
  d=d-1
  println(s"(- -b * a + c *d - -) = $one") 

  println(s"a++= $a")
  a+=1

  var g = 1
  val exp1= -2*(g-k)+c
  println(s"Value of -2*(g-k)+c= $exp1") 

  println(s"c=c++ $c")
  c+=1

  c+=1
  val expr=c*a
  println(s"c=++c*a++ $expr")
  a+=1
}