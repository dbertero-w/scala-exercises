object chapter_3_2 {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[T](tree: Tree[T]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(a, b) => 1 + size(a) + size(b)
    }
  }

  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(a) => a
      case Branch(a, b) => maximum(a).max(maximum(b))
    }
  }

  def depth[T](tree: Tree[T]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(a, b) => (1 + depth(a)) max (1 + depth(b))
    }
  }

  def map[T](tree: Tree[T], funct: T => T): Tree[T] = {
    tree match {
      case Leaf(a) => Leaf(funct(a))
      case Branch(a, b) => Branch(map(a, funct), map(b, funct))
    }
  }

  def fold[T, X](tree: Tree[T])(funct: Tree[T] => X): X = {
    funct(tree)
  }

  def main(args: Array[String]): Unit = {
    val test_tree = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))

    println(size(test_tree))
    println(maximum(test_tree))
    println(depth(test_tree))
    println(fold(test_tree)(depth))
    println(map(test_tree, (x: Int) => x + 1))
  }
}
