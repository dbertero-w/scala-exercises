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

  def listLenFoldLeft[T](xs: List[T]): Int = {
    foldLeft(xs, 0)((h, _) => h + 1)
  }

  def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = {
    xs match {
      case Nil => z
      case a :: as => foldLeft(as, f(z, a))(f)
    }
  }

  def sumFoldLeft(xs: List[Int]): Int = {
    xs.foldLeft(0)((a, b) => a + b)
  }

  def prodFoldLeft(xs: List[Int]): Int = {
    xs.foldLeft(1)((a, b) => a * b)
  }

  def reverse[T](xs: List[T]): List[T] = {
    xs.foldLeft(Nil)((as: List[T], a) => a :: as)
  }

  def reverseRight[T](xs: List[T]): List[T] = {
    xs.foldRight(Nil)((a: T, as: List[T]) => {
      as match {
        case Nil => a :: Nil
        case b :: bs => as ++ (a :: Nil)
      }
    })
  }

  def newFoldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = {
    def newf(a: A, b: B) = f(b, a)
    reverseRight(xs).foldRight(z)(newf)
  }

  def append[T](xs: List[T], v: T): List[T] = {
    xs.foldRight(List(v))((a, b) => b match {
      case Nil => Nil
      case bs => a :: bs
    })
  }

  def concatenate[T](xs: List[List[T]]): List[T] = {
    xs.foldRight(Nil)((a: List[T], as: List[T]) => as match {
      case Nil => a
      case bs => a ++ bs
    })
  }

  def main(args: Array[String]): Unit = {
    val testList = List(1, 2, 3, 4, 5)
    val emptyList = List()
    val singletonList = List(0)

    println(tail(testList))
    println(setHead(testList, 0))
    println(drop(testList, 3))
    println(dropWhile(testList, (a: Int) => a <= 3))
    println(init(testList))
    println(listLenFoldLeft(testList))
    println(listLenFoldLeft(emptyList))
    println(listLenFoldLeft(singletonList))
    println(sumFoldLeft(testList))
    println(prodFoldLeft(testList))
    println(reverse(testList))
    println(reverse(singletonList))
    println(reverse(emptyList))
    println(newFoldLeft(testList, Nil)((as: List[Int], a) => a :: as))
    println(append(testList, -12))
    println(concatenate(List(testList, testList, testList)))
  }
}
