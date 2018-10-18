package fintech.homework04
import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

  "size" should "count correctly" in {
    val b = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.size(b) should be (5)
  }

  "max" should "work correctly" in {
    val a = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.max(a) should be (3)

    val b = Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))
    Tree.max(b) should be (3)
  }

  "depth" should "work correctly" in {
    val a = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.depth(a) should be (3)

    val b = Branch(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))), Leaf(5))
    Tree.depth(b) should be (5)

    val c = Leaf(0)
    Tree.depth(c) should be (1)
  }

  "map" should "change each element" in {
    val b = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val f = (x: Int) => x + 1
    val tree = Tree.map(b)(f).asInstanceOf[Branch[Int]]

    val l = tree.left.asInstanceOf[Branch[Int]]
    l.left.asInstanceOf[Leaf[Int]].value should be (5)
    l.right.asInstanceOf[Leaf[Int]].value should be (4)

    val r = tree.right.asInstanceOf[Branch[Int]]
    r.left.asInstanceOf[Leaf[Int]].value should be (3)
    r.right.asInstanceOf[Leaf[Int]].value should be (2)
  }
}
