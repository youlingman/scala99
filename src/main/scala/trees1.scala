/**
  * Created by Administrator on 2016-4-19.
  */
package binarytree {

  sealed abstract class Tree[+T] {
    /* P56 (**) Symmetric binary trees.
        Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Add an isSymmetric method to the Tree class to check whether a given binary tree is symmetric. Hint: Write an isMirrorOf method first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.
        scala> Node('a', Node('b'), Node('c')).isSymmetric
        res0: Boolean = true
     */
    def isSymmetric: Boolean

    def isMirrorOf[V](n: Tree[V]): Boolean

    def isHeightBalanced: Boolean

    def isLeaf: Boolean

    def height: Int

    def nodes: Int

    def addValue[U >: T](x: U)(implicit ev: U => Ordered[U]): Tree[U]

    def preorderFold[B](z: B)(op: (B, Tree[T]) => B): B

    def inorderFold[B](z: B)(op: (B, Tree[T]) => B): B

    def postorderFold[B](z: B)(op: (B, Tree[T]) => B): B

    def preorder: List[T]

    def inorder: List[T]

    def atLevel(level: Int): List[T]

    def layoutBinaryTree: Tree[T] = layoutBinaryTreeInternal(1, 1)._1

    def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[T], Int)

    def layoutBinaryTree2: Tree[T]

    def layoutBinaryTreeInternal2(x: Int, depth: Int, level: Int): Tree[T]

    def layoutBinaryTree3: Tree[T]

    def layoutBinaryTreeInternal3(x: Int, depth: Int, level: Int): Tree[T]

    def toDotString: String
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    //    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
    override def toString = (left, right) match {
      case (End, End) => value.toString
      case (_, _) => value.toString + "(" + left.toString + "," + right.toString + ")"
    }

    def isSymmetric: Boolean = left.isMirrorOf(right)

    def isMirrorOf[V](n: Tree[V]): Boolean = n match {
      case Node(_, l, r) => left.isMirrorOf(r) && right.isMirrorOf(l)
      case _ => false
    }

    def addValue[U >: T](x: U)(implicit ev: U => Ordered[U]): Tree[U] =
      if (x < value) Node(value, left.addValue(x), right)
      else Node(value, left, right.addValue(x))

    def isHeightBalanced: Boolean = left.isHeightBalanced && right.isHeightBalanced && Math.abs(left.height - right.height) <= 1

    def height: Int = 1 + Math.max(left.height, right.height)

    def nodes: Int = left.nodes + right.nodes + 1

    def isLeaf: Boolean = (left, right) match {
      case (End, End) => true
      case _ => false
    }

    def preorderFold[B](z: B)(op: (B, Tree[T]) => B): B = {
      var result = z
      result = op(result, this)
      result = left.preorderFold(result)(op)
      result = right.preorderFold(result)(op)
      result
    }

    def inorderFold[B](z: B)(op: (B, Tree[T]) => B): B = {
      var result = z
      result = left.inorderFold(result)(op)
      result = op(result, this)
      result = right.inorderFold(result)(op)
      result
    }

    def postorderFold[B](z: B)(op: (B, Tree[T]) => B): B = {
      var result = z
      result = left.postorderFold(result)(op)
      result = right.postorderFold(result)(op)
      result = op(result, this)
      result
    }

    def preorder = preorderFold(List(): List[T]) {
      case (l, n: Node[T]) => l ::: List(n.value)
      case (l, End) => l
    }

    def inorder = inorderFold(List(): List[T]) {
      case (l, n: Node[T]) => l ::: List(n.value)
      case (l, End) => l
    }

    def leafCount: Int = postorderFold(0) { (n, t) => if (t.isLeaf) n + 1 else n }

    def leafList: List[T] = postorderFold(List(): List[T]) {
      case (l, n: Node[T]) => if (n.isLeaf) n.value :: l else l
      case (l, End) => l
    }

    def internalList: List[T] = postorderFold(List(): List[T]) {
      case (l, n: Node[T]) => if (n.isLeaf) l else n.value :: l
      case (l, End) => l
    }

    def atLevel(level: Int): List[T] = level match {
      case 0 => Nil
      case 1 => List(value)
      case _ => left.atLevel(level - 1) ::: right.atLevel(level - 1)
    }

    def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[T], Int) = {
      val (l, x_1) = left.layoutBinaryTreeInternal(x, depth + 1)
      val (r, x_2) = right.layoutBinaryTreeInternal(x_1 + 1, depth + 1)
      (PositionedNode(value, l, r, x_1, depth), x_2)
    }

    override def layoutBinaryTree2: Tree[T] = {
      layoutBinaryTreeInternal2(rootX(Math.pow(2, height - 1).toInt), 1, height)
    }

    /*
      compute the x for current node in layoutBinaryTree2
     */
    def rootX(x: Int): Int = left match {
      case l: Node[T] => l.rootX(x / 2) + x / 2
      case End => 1
    }

    override def layoutBinaryTreeInternal2(x: Int, depth: Int, level: Int): Tree[T] = {
      val half = Math.pow(2, level - 1).toInt / 2
      val l = left.layoutBinaryTreeInternal2(x - half, depth + 1, level - 1)
      val r = right.layoutBinaryTreeInternal2(x + half, depth + 1, level - 1)
      PositionedNode(value, l, r, x, depth)
    }

    override def layoutBinaryTree3: Tree[T] = ???

    override def layoutBinaryTreeInternal3(x: Int, depth: Int, level: Int): Tree[T] = ???

    def toDotString: String = value.toString + left.toDotString + right.toDotString
  }

  case object End extends Tree[Nothing] {
    override def toString = "."

    //    override def toString = ""

    def isSymmetric: Boolean = true

    def isMirrorOf[V](n: Tree[V]): Boolean = n == End

    def addValue[U >: Nothing](x: U)(implicit ev: U => Ordered[U]): Tree[U] = Node(x)

    def isHeightBalanced: Boolean = true

    def height: Int = 0

    def nodes: Int = 0

    def isLeaf: Boolean = false

    def preorderFold[B](z: B)(op: (B, Tree[Nothing]) => B): B = z

    def inorderFold[B](z: B)(op: (B, Tree[Nothing]) => B): B = z

    def postorderFold[B](z: B)(op: (B, Tree[Nothing]) => B): B = z

    def preorder = Nil

    def inorder = Nil

    def atLevel(level: Int): List[Nothing] = Nil

    def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[Nothing], Int) = (End, x)

    override def layoutBinaryTree2: Tree[Nothing] = End

    override def layoutBinaryTreeInternal2(x: Int, depth: Int, level: Int): Tree[Nothing] = End

    override def layoutBinaryTree3: Tree[Nothing] = End

    override def layoutBinaryTreeInternal3(x: Int, depth: Int, level: Int): Tree[Nothing] = End

    def toDotString: String = "."
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }


  class PositionedNode[+T](override val value: T, override val left: Tree[T], override val right: Tree[T], x: Int, y: Int) extends Node[T](value, left, right) {
    override def toString = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
  }

  object PositionedNode {
    def apply[T](value: T, left: Tree[T], right: Tree[T], x: Int, y: Int): PositionedNode[T] = new PositionedNode(value, left, right, x, y)
  }

}
