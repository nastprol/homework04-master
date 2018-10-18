package fintech.homework04

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // реализовать функцию fold

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case l: Leaf[A] => f(l.value)
      case b: Branch[A] => g(fold(b.right)(f)(g), fold(b.left)(f)(g))
    }
  }

  // реализовать следующие функции через fold

  def size[A](t: Tree[A]): Int = fold(t)((x: A) => 1)((x: Int, y: Int) => x + y + 1)

  def max(t: Tree[Int]): Int = fold(t)((x: Int) => x)((x: Int, y: Int) => math.max(x, y))

  def depth[A](t: Tree[A]): Int = fold(t)((x: A) => 1)((x: Int, y: Int) => math.max(x, y) + 1)

  //здесь возможно придется прибегнуть к насильному указанию типа: Leaf(a): Tree[A]
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](t)(x => Leaf(f(x)))((l, r) => Branch(l, r))

}
