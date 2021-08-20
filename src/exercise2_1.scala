object exercise2_1 {
  def fibonacci(n: Int): Int = {
    def loop(n: Int, acc1: Int, acc2: Int): Int = {
      if (n == 0)
        return acc1 + acc2

      loop(n - 1, acc2, acc1 + acc2)
    }

    if (n == 0)
      return 0

    if (n == 1)
      return 1

    loop(n - 2, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    def print_fib(n: Int): Unit = {
      if (n < 0)
        return

      println(fibonacci(n))
      print_fib(n - 1)
    }

    print_fib(10)
  }

}
