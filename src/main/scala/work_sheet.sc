import S99_binarytree.Tree
import S99_Miscellaneous.{Point, Sudoku}
import binarytree.{End, Node}
import multiwaytree.MTree
import graph.{Digraph, Graph}

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
  //  MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).internalPathLength
  //  MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).postorder
  //  MTree("a", List(MTree("b", List(MTree("c"))))).lispyTree
  //  Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").findCycles("f")
  //  Graph.fromString("[a-b, b-c, a-c]").nodes
  //  Graph.fromString("[a-b, b-c, a-c]").edges
  //  graph.Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").toAdjacentForm.toSet
  //  Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").toAdjacentForm.toMap.values.map(_.toSet).toSet
  //  List(("m",List(("q","7"))), ("p",List(("m","5"), ("q","9"))), ("k",List()), ("q",List())).toMap.values.map(_.toSet).toSet
  //  List(("m",List(("q","7"))), ("p",List(("m","5"), ("q","9"))), ("k",List()), ("q",List())).toSet
  //  Graph.fromString("[a-b, b-c, a-c]").spanningTrees
  //  Graph.fromString("[a-b, b-c, a-c, a-d]").nodesByDegree
  //  Graph.fromString("[a-b, b-c, e, a-c, a-d]").nodesByDepthFrom("d")
  //  Graph.fromString("[a-b, c]").splitGraph.foreach(_.nodes.foreach(println(_)))
  //  Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").nodesByDepthFrom("g")
  //  Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").nodes.foreach(println(_))
  //  Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").edges.foreach(println(_))
  //  Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").splitGraph
  //  Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").splitGraph.foreach {
  //    n =>
  //      println("---end edges")
  //      n.edges.foreach(println(_))
  //  }
  //  Graph.fromString("[g-h, h-g]").edges.foreach{
  //    println("---end edges")
  //    println(_)
  //  }
  //  Graph.fromString("[a-b, b-c, a-c]").spanningTrees.foreach{
  //    g =>
  //      println("----")
  //      g.nodes.values.map{
  //        n=>
  //          println(n)
  //          println(n.adj)
  //      }
  //  }
  //  Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").splitGraph.foreach(_.edges.foreach(println(_)))
  //  Digraph.fromStringLabel("[a>b, c>a, d>b]").splitGraph.size
  //    Digraph.fromStringLabel("[a>b, c>a, d>b]").splitGraph.foreach{
  //      println("---")
  //      _.edges.foreach(println(_))
  //    }
  //  Graph.fromString("[a-b, b-c, a-c, a-d]").nodesByDegree
  //  Graph.fromString("[a-b, b-c, a-c]").spanningTrees.size
  //  Graph.fromString("[a-b, b-c, a-c]").isTree
  //  Graph.fromString("[a-b, b-c, a-c, a-d]").colorNodes
  //    Digraph.fromStringLabel("[a>b, c>a, d>b]").colorNodes
  //  Digraph.fromStringLabel("[a>b, a>c, a>d, e>d]").colorNodes
  //  Graph.fromString("[a-b]").isIsomorphicTo(Graph.fromString("[5-7]"))
  //  S99_Miscellaneous.eightQueens(3)
  //  S99_Miscellaneous.knightTour(4, S99_Miscellaneous.Point(0, 0))
  //  S99_Miscellaneous.vonKochConjecture(Graph.fromString("[b-c, f-c]"))
  //  S99_Miscellaneous.isIdentifier("abc_123")
  //  S99_Miscellaneous.arithmeticPuzzle(List(2, 3, 5, 7, 11))
  //  S99_Miscellaneous.genMatrix(9, 9)
  val s = S99_Miscellaneous.Sudoku(Map(Point(1, 3) -> "4", Point(1, 4) -> "8", Point(1, 8) -> "1", Point(1, 9) -> "7", Point(2, 1) -> "6", Point(2, 2) -> "7", Point(2, 4) -> "9", Point(3, 1) -> "5", Point(3, 3) -> "8", Point(3, 5) -> "3", Point(3, 9) -> "4", Point(4, 1) -> "3", Point(4, 4) -> "7", Point(4, 5) -> "4", Point(4, 7) -> "1", Point(5, 2) -> "6", Point(5, 3) -> "9", Point(5, 7) -> "7", Point(5, 8) -> "8", Point(6, 3) -> "4", Point(6, 5) -> "6", Point(6, 6) -> "9", Point(6, 9) -> "5", Point(7, 1) -> "1", Point(7, 5) -> "8", Point(7, 7) -> "3", Point(7, 9) -> "6", Point(8, 6) -> "6", Point(8, 8) -> "9", Point(8, 9) -> "1", Point(9, 1) -> "2", Point(9, 2) -> "4", Point(9, 6) -> "1", Point(9, 7) -> "5"))
  S99_Miscellaneous.Sudoku.solve(s)
  //  S99_Miscellaneous.Sudoku(Sudoku(Map()).matrix)
}
