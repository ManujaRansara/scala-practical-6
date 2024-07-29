object Inventories{
  case class Product(name:String, quantity:Int , price:Double)

  var inventory1:Map[Int, Product]=Map(
    10->Product("Laptops", 15, 200000),
    20->Product("Mouse", 10, 1500),
    30->Product("Keyboard", 5, 2000)
  )

  var inventory2:Map[Int, Product]=Map(
    20 -> Product("Mouse", 15, 1000),
    30 -> Product("Keyboard", 2, 2500)
  )

  def Retrieve(inventory: Map[Int, Product]): List[String]={
    inventory.values.map(_.name).toList
  }

  def CalculateTotal(inventory:Map[Int, Product]): Double={
    inventory.values.map(product => product.quantity * product.price).sum
  }

  def Empty(inventory:Map[Int, Product]): Boolean={
    inventory.isEmpty
  }

  def Merge(inventory1:Map[Int, Product], inventory2: Map[Int, Product]): Map[Int, Product]={
    inventory2.foldLeft(inventory1) {(acc, entry) =>
      val (key , newProduct) = entry
      acc.get(key) match {
        case Some(existingProduct) =>
          acc.updated(key, Product(existingProduct.name, existingProduct.quantity + newProduct.quantity, math.max(existingProduct.price, newProduct.price)))
        case None =>
          acc + (key -> newProduct)
      }
    }
  }

  def Details(inventory: Map[Int, Product], productId: Int):Unit={
    inventory.get(productId) match {
      case Some(product) => println(s"Product ID: $productId, Name: ${product.name}, Quantity: ${product.quantity}, Price: ${product.price}")
      case None => println(s"Product with ID $productId does not exist.")
    }
  }

  def main(args: Array[String]):Unit={

    println("\n1.Retrieve all product names from inventory1:")
    println(Retrieve(inventory1))
    println("\n2.Calculate the total value of all products in inventory1:")
    println(CalculateTotal(inventory1))
    println("\n3.Check if inventory1 is empty:")
    println(Empty(inventory1))
    println("\n4.Merge inventory1 and inventory2:")
    val merge = Merge(inventory1, inventory2)
    merge.foreach { case (id, product) =>
      println(s"Product ID:$id , Name:${product.name} , Quantity:${product.quantity} , Price:${product.price}")
    }
    println("\n5.Check if a product with ID 10 exists and print its details:")
    Details(inventory1, 10)
  }
}

