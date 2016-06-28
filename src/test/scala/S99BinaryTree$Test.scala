import S99_binarytree.Tree
import binarytree.{End, Node}
import org.scalatest.FunSuite

/**
  * Created by Administrator on 2016-4-20.
  */
class S99BinaryTree$Test extends FunSuite {
  test("P55") {
    assert(Tree.cBalanced(1, "x") == List(Node("x", End, End)))
    assert(Tree.cBalanced(2, "x") == List(Node("x", End, Node("x", End, End)), Node("x", Node("x", End, End), End)))
    assert(Tree.cBalanced(3, "x") == List(Node("x", Node("x", End, End), Node("x", End, End))))
  }

  test("P56") {
    assert(Node('a', Node('b'), Node('c')).isSymmetric)
    assert(Node('a').isSymmetric)
    assert(!Node("x", End, Node("x", End, End)).isSymmetric)
    assert(Node("x", Node("x", Node("x", End, End), End), Node("x", End, Node("x", End, End))).isSymmetric)
    assert(Node("x", Node("x", End, Node("x", End, End)), Node("x", Node("x", End, End), End)).isSymmetric)
  }

  test("P57") {
    assert(End.addValue(2) == Node(2))
    assert(Node(2).addValue(3) == Node(2, End, Node(3)))
    assert(Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric)
    assert(!Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric)
  }

  test("P58") {
    assert(Tree.symmetricBalancedTrees(1, "x") == List(Node("x", End, End)))
    assert(Tree.symmetricBalancedTrees(2, "x") == Nil)
    assert(Tree.symmetricBalancedTrees(3, "x") == List(Node("x", Node("x", End, End), Node("x", End, End))))
    assert(Tree.symmetricBalancedTrees(4, "x") == Nil)
    assert(Tree.symmetricBalancedTrees(5, "x") == List(Node("x", Node("x", End, Node("x", End, End)), Node("x", Node("x", End, End), End)), Node("x", Node("x", Node("x", End, End), End), Node("x", End, Node("x", End, End)))))
  }

  test("P59") {
    assert(Tree.hbalTrees(0, "x").length == 1)
    assert(Tree.hbalTrees(1, "x").length == 1)
    assert(Tree.hbalTrees(2, "x").length == 3)
    assert(Tree.hbalTrees(3, "x").length == 15)
    assert(Tree.hbalTrees(4, "x").length == 315)
  }

  test("P60") {

  }

  test("P61") {
    assert(Node('x', Node('x'), End).leafCount == 1)
    assert(Node('x', Node('x'), Node('x')).leafCount == 2)
    assert(Node('x', Node('x'), Node('x')).leafCount == 2)
    assert(Node('x', Node('x', Node('x'), Node('x')), Node('x', Node('x'), Node('x'))).leafCount == 4)
    assert(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList.sorted == List('b', 'd', 'e'))
  }

  test("P62") {
    assert(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList == List('a', 'c'))
    assert(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2) == List('b', 'c'))
  }


  test("P63") {
    assert(Tree.completeBinaryTree(1, 'x') == Node('x'))
    assert(Tree.completeBinaryTree(3, 'x') == Node('x', Node('x'), Node('x')))
    assert(Tree.completeBinaryTree(4, 'x') == Node('x', Node('x', Node('x'), End), Node('x')))
    assert(Tree.completeBinaryTree(6, 'x') == Node('x', Node('x', Node('x'), Node('x')), Node('x', Node('x'), End)))
  }

  test("P64") {
    assert(Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree.toString == "T[3,1](a T[1,2](b . T[2,3](c . .)) T[4,2](d . .))")
  }

  test("P65") {
    assert(Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree2.toString == "T[3,1](a T[1,2](b . T[2,3](c . .)) T[5,2](d . .))")
  }

  test("P66") {

  }

  test("P67") {
    assert(Tree.fromString("a(b(d,e),c(,f(g,)))") == Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))))
    assert(Tree.fromString("a(b,c)") == Node('a', Node('b'), Node('c')))
    assert(Tree.fromString("a") == Node('a'))
    assert(Tree.fromString("") == End)
  }

  test("P68") {
    assert(Tree.fromString("a(b(d,e),c(,f(g,)))").preorder == List('a', 'b', 'd', 'e', 'c', 'f', 'g'))
    assert(Tree.fromString("a(b(d,e),c(,f(g,)))").inorder == List('d', 'b', 'e', 'a', 'c', 'g', 'f'))
    assert(Tree.preInTree(List('a', 'b', 'd', 'e', 'c', 'f', 'g'), List('d', 'b', 'e', 'a', 'c', 'g', 'f')) == Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))))
  }

  test("P69") {
    assert(Tree.fromDotstring("abd..e..c.fg...") == Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))))
    assert(Tree.fromDotstring("ab..c..") == Node('a', Node('b'), Node('c')))
    assert(Tree.fromDotstring("a..") == Node('a'))
    assert(Tree.fromDotstring("") == End)
  }
}
