import scala.io.StdIn.readLine
import scala.io.StdIn.readInt

object inventory_management {
    def main(args: Array[String]): Unit ={
        

        def getProductList(n: List[String]): List[String] = {
            val item = readLine("Enter name or 'done' to stop adding list: ")
            item match {
                case "done" => n
                case _ => getProductList(n :+ item)
            }
        }

        def printProductList(index: Int,n:List[String]): Unit = if(!n.isEmpty)
            {println(s"$index:${n.head}");
            printProductList(index + 1,n.tail)}
            else{println("Enter the correct list")}

        def getTotalProducts(n:List[String]): Unit = {
            println(s"The total number of list entered is: ${n.length}")

        }

        var item = List[String]();
        item = getProductList(item)
        printProductList(1,item)
        getTotalProducts(item)



    }
}