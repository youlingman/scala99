import S99_binarytree.Tree
import binarytree.{End, Node}
import multiwaytree.MTree

object work_sheet {
  // S99_arithmetic.printGoldbachList(9 to 20)
  //S99_arithmetic.printGoldbachListLimited(1 to 2000, 50)
  //S99_logic.table2((a: Boolean, b: Boolean) => S99_logic.and(a, S99_logic.or(a, b)))
  //S99_logic.gray(3)
  //S99_logic.gray(4)
  //S99_logic.huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))
  //S99_binarytree.Tree.cBalanced(5, "x")
  //Tree.symmetricBalancedTrees(5, "x")
  //Tree.allTrees(3, "x")
  //  Tree.hbalTrees(2, "x")
  //  Tree.hbalTrees(3, "x")
//  Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList
//  Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList
//  Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree
//  Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree2
//  Tree.fromList(List('n','k','m','c','a','e','d','g','u','p','q')).layoutBinaryTree2
//  Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))).toString
//  Tree.fromString("a(b(d,e),c(,f(g,)))")
//  Tree.fromString("a")
//  Tree.fromString("a(b(d,e),c(,f(g,)))").preorder
//  Tree.fromString("a(b(d,e),c(,f(g,)))").inorder
//  Tree.preInTree(List('a', 'b', 'd', 'e', 'c', 'f', 'g'), List('d', 'b', 'e', 'a', 'c', 'g', 'f'))
//  Tree.fromString("a(b(d,e),c(,f(g,)))").toDotString
//  Tree.fromDotstring("a..")
//  MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).toString
//  S99_multiwaytree.string2MTree("a^")
//  S99_multiwaytree.string2MTree("af^^")
//  S99_multiwaytree.string2MTree("afg^^c^bd^e^^^")
//  S99_multiwaytree.string2MTree("afg^^c^^")
//  MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e')))))
//  S99_multiwaytree.string2MTree("afg^^c^bd^e^^^") == MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e')))))
  MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).internalPathLength
  MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).postorder
  MTree("a", List(MTree("b", List(MTree("c"))))).lispyTree
}