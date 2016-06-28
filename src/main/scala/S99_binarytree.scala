import binarytree.{End, Tree, Node}

/**
  * Created by Administrator on 2016-4-19.
  */

object S99_binarytree {

  object Tree {
    /* P55 (**) Construct completely balanced binary trees.
        In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.
        Define an object named Tree. Write a function Tree.cBalanced to construct completely balanced binary trees for a given number of nodes. The function should generate all solutions. The function should take as parameters the number of nodes and a single value to put in all of them.

        scala> Tree.cBalanced(4, "x")
        res0: List(Node[String]) = List(T(x T(x . .) T(x . T(x . .))), T(x T(x . .) T(x T(x . .) .)), ...
    */
    def cBalanced[T](n: Int, value: T): List[Tree[T]] =
      if (n == 0) List(End)
      else for {
        i <- List.range((n - 1) / 2, (n + 2) / 2)
        left <- cBalanced(i, value)
        right <- cBalanced(n - 1 - i, value)
      } yield Node(value, left, right)

    def fromList[T](list: List[T])(implicit ev: T => Ordered[T]): Tree[T] = list.foldLeft(End: Tree[T]) { (node, n) => node.addValue(n) }


    def symmetricBalancedTrees[T](n: Int, value: T): List[Tree[T]] = cBalanced(n, value).filter(_.isSymmetric)

    def allTrees[T](n: Int, value: T): List[Tree[T]] = n match {
      case 0 => List(End)
      case _ => for {
        i <- List.range(0, n)
        l <- allTrees(i, value)
        r <- allTrees(n - 1 - i, value)
      } yield Node(value, l, r)
    }

    def hbalTrees[T](h: Int, value: T): List[Tree[T]] = h match {
      case 0 => List(End)
      case 1 => List(Node(value))
      case _ => for {
        l_h <- List.range(h - 2, h)
        r_h <- List.range(h - 2, h)
        if l_h == h - 1 || r_h == h - 1
        l <- hbalTrees(l_h, value)
        r <- hbalTrees(r_h, value)
      } yield Node(value, l, r)
    }

    def hbalTreesWithNodes[T](n: Int, value: T): List[Tree[T]] = (minHbalHeight(n) to maxHbalHeight(n)).flatMap(hbalTrees(_, value)).filter(_.nodes == n).toList

    def completeBinaryTree[T](n: Int, value: T): Tree[T] = {
      def _completeBinaryTree(n: Int, tag: Int, value: T): Tree[T] = {
        if (tag > n) End
        else Node(value, _completeBinaryTree(n, tag * 2, value), _completeBinaryTree(n, tag * 2 + 1, value))
      }
      _completeBinaryTree(n, 1, value)
    }

    def fromString(str: String): Tree[Char] = {
      def split(s: String, pos: Int) = (s.take(pos), s.drop(pos).tail)
      def partition(s: String, nest: Int, pos: Int): (String, String) = {
        if (pos == s.length) split(s, pos)
        else
          s.charAt(pos) match {
            case '(' => partition(s, nest + 1, pos + 1)
            case ')' => partition(s, nest - 1, pos + 1)
            case ',' => if (nest == 0) split(s, pos) else partition(s, nest, pos + 1)
            case _ => partition(s, nest, pos + 1)
          }
      }
      str.length match {
        case 0 => End
        case 1 => Node(str.head)
        case _ =>
          //          val (leftStr, rightStr) = partition(str.substring(2).dropRight(1), 0, 0)
          val (leftStr, rightStr) = partition(str.substring(str.indexOf('(') + 1).dropRight(1), 0, 0)
          Node(str.head, fromString(leftStr), fromString(rightStr))
      }
    }

    def preInTree[T](preSeq: List[T], inSeq: List[T]): Tree[T] = {
      if (preSeq.isEmpty && inSeq.isEmpty) End
      else if (preSeq.length == 1 && inSeq.length == 1 && preSeq.head == inSeq.head) Node(preSeq.head)
      else {
        val pos = inSeq.indexOf(preSeq.head)
        Node(preSeq.head, preInTree(preSeq.slice(1, pos + 1), inSeq.take(pos)), preInTree(preSeq.drop(pos + 1), inSeq.drop(pos + 1)))
      }
    }

    def fromDotstring(str: String): Tree[Char] = {
      def partition(s: String, nest: Int, pos: Int): (String, String) = {
        if (nest == -1) (s.take(pos), s.drop(pos))
        else
          s.charAt(pos) match {
            case '.' => partition(s, nest - 1, pos + 1)
            case _ => partition(s, nest + 1, pos + 1)
          }
      }
      str match {
        case "" => End
        case "." => End
        case _ =>
          val (leftStr, rightStr) = partition(str.tail, 0, 0)
          Node(str.head, fromDotstring(leftStr), fromDotstring(rightStr))
      }
    }

  }

  def minHbalNodes(h: Int): Int = h match {
    case 0 => 0
    case 1 => 1
    case _ => minHbalNodes(h - 2) + minHbalNodes(h - 1) + 1
  }

  def maxHbalNodes(h: Int): Int = Math.pow(2, h).toInt - 1

  def minHbalHeight(n: Int): Int = n match {
    case 0 => 0
    case _ => minHbalHeight(n / 2) + 1
  }

  def maxHbalHeight(n: Int): Int = Stream.from(1).takeWhile(minHbalNodes(_) <= n).last
}
