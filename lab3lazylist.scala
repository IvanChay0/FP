import scala.concurrent.{Future, ExecutionContext}
import scala.util.Random
import scala.collection.mutable
import scala.io.StdIn

// Модели данных
case class Product(name: String, secretCode: Int, price: Double, weight: Double)
case class Customer(id: Int)
case class Cashier(id: Int)
case class ShopConfig(maxQueueSize: Int, numCashiers: Int, numCustomers: Int, workTime: Int)

// События симуляции
sealed trait Event
case object Tick extends Event
case class AddEvent(customerId: Int, product: Product) extends Event
case class ProcessEvent(cashierId: Int, product: Product) extends Event


object SimpleShopSimulation {

  // Потокобезопасная очередь 
  class ThreadSafeQueue[A] {
    private val queue = mutable.Queue[A]()

    def enqueue(item: A): Int = queue.synchronized {
      queue.enqueue(item)
      queue.size
    }

    def dequeue(): Option[A] = queue.synchronized {
      if (queue.nonEmpty) Some(queue.dequeue()) else None
    }

    def size: Int = queue.synchronized(queue.size)
    def isEmpty: Boolean = queue.synchronized(queue.isEmpty)
  }

  def main(args: Array[String]): Unit = {
    implicit val ec: ExecutionContext = ExecutionContext.global

    println("Введите максимальный размер очереди: ")
    val maxQueueSize = StdIn.readInt()

    println("Введите количество кассиров: ")
    val numCashiers = StdIn.readInt()

    println("Введите количество покупателей: ")
    val numCustomers = StdIn.readInt()


    println("Введите время работы магазина (сек): ")
    val workTime = LazyList.continually(StdIn.readLine())
      .map(line => {
        try {
          val time = line.toInt
          if (time > 0) Some(time) else {
            println("Ошибка: время должно быть > 0. Попробуйте снова:")
            None
          }
        } catch {
          case _: NumberFormatException =>
            println("Ошибка: введите число. Попробуйте снова:")
            None
        }
      })
      .collectFirst { case Some(time) => time } // берем первое корректное значение
      .getOrElse {
        println("Ошибка ввода. Устанавливаю время по умолчанию: 30 сек")
        30
      }

    val config = ShopConfig(maxQueueSize, numCashiers, numCustomers, workTime)
    runSimulation(config)
  }

  def runSimulation(config: ShopConfig): Unit = {
    implicit val ec: ExecutionContext = ExecutionContext.global

    val availableProducts = List(
      Product("Телевизор", 1, 5400.0, 3.2),
      Product("Хлеб", 2, 45.0, 0.5),
      Product("Смартфон", 3, 8900.0, 0.3),
      Product("Молоко", 4, 85.0, 1.0)
    )

    val customers = (1 to config.numCustomers).map(Customer).toList
    val cashiers = (1 to config.numCashiers).map(Cashier).toList

    val queue = new ThreadSafeQueue[Product]()
    val rng = new Random()

    val startTime = System.currentTimeMillis()
    val endTime = startTime + config.workTime * 1000L

    // Генерация потока событий с помощью LazyList
    val eventStream: LazyList[Event] = LazyList.unfold((0, rng)) { case (step, rng) =>
      if (System.currentTimeMillis() >= endTime) {
        None
      } else {
        val shouldAdd = queue.size < config.maxQueueSize && rng.nextDouble() > 0.3
        val shouldProcess = !queue.isEmpty

        val (event, newRng) =
          if (shouldAdd) {
            val customer = customers(rng.nextInt(customers.length))
            val product = availableProducts(rng.nextInt(availableProducts.length))
            (AddEvent(customer.id, product), new Random(rng.nextLong()))
          } else if (shouldProcess) {
            val cashier = cashiers(rng.nextInt(cashiers.length))
            queue.dequeue() match {
              case Some(product) =>
                (ProcessEvent(cashier.id, product), new Random(rng.nextLong()))
              case None =>
                (Tick, rng)
            }
          } else {
            (Tick, rng)
          }

        Some((event, (step + 1, newRng)))
      }
    }

    // Обработка событий
    eventStream
      .takeWhile(_ => System.currentTimeMillis() < endTime)
      .foreach {
        case AddEvent(customerId, product) =>
          val newSize = queue.enqueue(product)
          println(s"Покупатель-$customerId добавил товар: ${product.name} (очередь: $newSize)")
          Thread.sleep(100 + rng.nextInt(150))

        case ProcessEvent(cashierId, product) =>
          val currentSize = queue.size
          println(s"Кассир-$cashierId обработал товар: ${product.name} (осталось: $currentSize)")
          Future {
            Thread.sleep(500 + rng.nextInt(300))
            println(s"Кассир-$cashierId подсчитал товар: ${product.name} - цена: ${product.price}, вес: ${product.weight}")
          }

        case Tick =>
          Thread.sleep(50)
      }

    // Завершение работы
    Thread.sleep(1000) // даём время асинхронным подсчётам завершиться
    println("\nМагазин закрывается...")
    (1 to config.numCustomers).foreach(id => println(s"Покупатель-$id ушел из магазина"))
    (1 to config.numCashiers).foreach(id => println(s"Кассир-$id закончил работу"))
    println("Работа магазина завершена")
  }
}