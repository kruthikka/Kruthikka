@main def main() :Unit=
  var area = calculate_area(5)
  var fah = covert_to_fahrenheit(35)
  var cost = total_cost(60)
  var volume = calculate_volume(5)
  var time = total_time(2,3)
  
  println(s"The area of disk is $area ")
  println(s"The Temperature in F is $fah")
  println(s"The volume of sphere is $volume")
  println(s"The total cost of book is $cost")
  println(s"The total runtime is $time") 


def calculate_area(r:Double):Double= math.Pi*r*r

def covert_to_fahrenheit(c:Double):Double =c*1.8+32

def calculate_volume(r:Double):Double = 4/3*math.Pi*r*r*r

def discount(b: Int):Double = b*24.95*0.6

def shippingcost(b:Int):Double = if(b>50) 50*3+(b-50)*0.75 else b*3

def total_cost(b:Int):Double = discount(b)+shippingcost(b)

def runtime_easypace(k:Int):Int =k*8

def runtime_Tempo(k:Int):Int=k*7

def total_time(ke:Int,kt:Int):Int = 2*runtime_easypace(ke)+ runtime_Tempo(kt)