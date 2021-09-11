import sun.font.TrueTypeFont

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

  def myMap[A, B](as: List[A])(f: A => B): List[B] = {
    as match {
      case Nil => Nil
      case x :: xs => f(x) :: myMap(xs)(f)
    }
  }

  def myFilter[A](as: List[A])(f: A => Boolean): List[A] = {
    myFlatMap(as)((x: A) => {
      if (f(x))
        List(x)
      else
        Nil
    })
    //    as match {
//      case Nil => Nil
//      case x :: xs => {
//        if (f(x))
//          x :: myFilter(xs)(f)
//        else
//          myFilter(xs)(f)
//      }
//    }
  }

  def myFlatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    as match {
      case Nil => Nil
      case x :: xs => f(x) ++ myFlatMap(xs)(f)
    }
  }

  def addOne(xs: List[Int]): List[Int] = {
    myMap(xs)((a: Int) => a + 1)
  }

  def stringify(xs: List[Double]): List[String] = {
    myMap(xs)((a: Double) => a.toString())
  }

  def zipList[A](as: List[A], bs: List[A]): List[List[A]] = {
    as match {
      case Nil => Nil
      case x :: xs => {
        bs match {
          case Nil => Nil
          case y :: ys => List(x, y) :: zipList(xs, ys)
        }
      }
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def checkSubsequence(as: List[A], bs: List[A]): Boolean = {
      as match {
        case Nil => {
          bs match {
            case Nil => true
            case _ => false
          }
        }
        case x :: xs => {
          bs match {
            case Nil => true
            case y :: ys => {
              if (x == y)
                checkSubsequence(xs, ys)
              else
                false
            }
          }
        }
      }
    }

    sup match {
      case Nil => {
        sub match {
          case Nil => true
          case ys => false
        }
      }
      case x :: xs => {
        checkSubsequence(x :: xs, sub) || hasSubsequence(xs, sub)
        }
      }
    }

  def main(args: Array[String]): Unit = {
    val testList = List(1, 2, 3, 4, 5)
    val testDouble = List(1.1, 2.3, 3.7, 4.9, 5.0)
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
    println(addOne(testList))
    println(stringify(testDouble))
    println(myFilter(testList)((x: Int) => x > 3))
    println(myFlatMap(testList)((x: Int) => List(x, x, x)))
    println(zipList(testList, testList))
    println(hasSubsequence(testList, List(4, 5)))
    println(hasSubsequence(testList, List(1, 3)))
    println(hasSubsequence(testList, List(1, 2, 3, 4)))

  }
}
