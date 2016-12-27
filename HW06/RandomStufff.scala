trait RandomStuffTrait {
  def transform(list: List[Int], op: (Int) => Int): List[Int]
  def allValid(list: List[Int], op: (Int) => Boolean): Boolean
  def executeWithRetry(retryCount: Int, op: => Int): Option[Int]
}

object RandomStuff extends RandomStuffTrait {
  def transform(list: List[Int], op: (Int) => Int): List[Int] = {
    var result: List[Int] = List()
    result = for (i <- list) yield op(i)
    return result
  }
  
  def allValid(list: List[Int], op: (Int) => Boolean): Boolean = {
    list.foreach(i => if (!op(i)) {
      return false
    })
    return true
  }

  def executeWithRetry(retryCount: Int, op: => Int): Option[Int] = {
    for (i <- 1 to retryCount) {
      try {
        return Option(op)
      } catch {
        case ex: Exception => {}
      }
    }
    None
  }
}
