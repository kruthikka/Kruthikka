@main def main4(): Unit = {
    def profit(price:Double)=
    {
    var attendance=120+((15-price)/5)*20
    var total_expense=attendance*3+500
    var total_income=price*attendance
    var profit=total_income-total_expense
    println(s"Profit for $price is Rs : $profit")
    }
    profit(15)
    profit(20)
    profit(10)
}