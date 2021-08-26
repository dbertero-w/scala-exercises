object exercise2_3 {
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    def partial(a: A): (B => C) = {
      (x: B) => f(a, x)
    }
    (a: A) => partial(a)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    println(curry((a: Int, b: Int) => a + b))
    println(uncurry(curry((a: Int, b: Int) => a + b))(2, 3))
    println(compose((a: Int) => a * 2, (b: Int) => b * 5)(2))
  }
}

