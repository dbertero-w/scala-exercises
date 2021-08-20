object exercise2_2 {
  def is_sorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1)
        true

      else if(!ordered(as(n), as(n + 1)))
        false

      else
        loop(n + 1)
    }

    loop(0)
  }

  def main(args: Array[String]): Unit = {
    println(is_sorted[Int](Array(1, 2, 3, 4, 5), (a, b) => {a <= b}))
    println(is_sorted[Int](Array(1, 2, 3, 19, 5), (a, b) => {a <= b}))


  }
}
