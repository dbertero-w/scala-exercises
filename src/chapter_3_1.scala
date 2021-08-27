object chapter_3_1 {
  def tail[T](xs: List[T]): List[T] = {
    xs match {
      case Nil => Nil
      case _ :: as => as
    }
  }

  def setHead[T](xs: List[T], head: T): List[T] = {
    head :: tail(xs)
  }

  def drop[T](xs: List[T], n: Int): List[T] = {
    if (n == 0)
      return xs

    xs match {
      case Nil => Nil
      case _ :: as => drop(as, n - 1)
    }
  }

  def dropWhile[T](xs: List[T], pred: (T => Boolean)): List[T] = {
    xs match {
      case Nil => Nil
      case a :: as => {
        if (pred(a))
          dropWhile(as, pred)
        else
          xs
      }
    }
  }

  def init[T](xs: List[T]): List[T] = {
    xs match {
      case Nil => Nil
      case a :: Nil => Nil
      case a :: as => a :: init(as)
    }
  }

  def main(args: Array[String]): Unit = {
    val testList = List(1, 2, 3, 4, 5)
    println(tail(testList))
    println(setHead(testList, 0))
    println(drop(testList, 3))
    println(dropWhile(testList, (a: Int) => a <= 3))
    println(init(testList))
  }
}
