import S99_Miscellaneous.{Nonograms, Sudoku, Point}
import org.scalatest.FunSuite

import graph.{Digraph, Graph}

/**
  * Created by Administrator on 2017-1-24.
  */
class S99Miscellaneous$Test extends FunSuite {

  test("P93") {

  }

  test("P96") {
    assert(S99_Miscellaneous.vonKochConjecture(Graph.fromString("[b-c, f-c]")))
    assert(!S99_Miscellaneous.vonKochConjecture(Graph.fromString("[b-c, f-c, b-f]")))
  }

  test("P97") {
    val s = Map(Point(1, 1) -> "9", Point(1, 2) -> "3", Point(1, 3) -> "4", Point(1, 4) -> "8", Point(1, 5) -> "2", Point(1, 6) -> "5", Point(1, 7) -> "6", Point(1, 8) -> "1", Point(1, 9) -> "7",
      Point(2, 1) -> "6", Point(2, 2) -> "7", Point(2, 3) -> "2", Point(2, 4) -> "9", Point(2, 5) -> "1", Point(2, 6) -> "4", Point(2, 7) -> "8", Point(2, 8) -> "5", Point(2, 9) -> "3",
      Point(3, 1) -> "5", Point(3, 2) -> "7", Point(3, 3) -> "2", Point(3, 4) -> "9", Point(3, 5) -> "1", Point(3, 6) -> "4", Point(3, 7) -> "8", Point(3, 8) -> "5", Point(3, 9) -> "3",
      Point(4, 1) -> "3", Point(4, 2) -> "2", Point(4, 3) -> "5", Point(4, 4) -> "7", Point(4, 5) -> "4", Point(4, 6) -> "8", Point(4, 7) -> "1", Point(4, 8) -> "6", Point(4, 9) -> "9",
      Point(5, 1) -> "4", Point(5, 2) -> "6", Point(5, 3) -> "9", Point(5, 4) -> "1", Point(5, 5) -> "5", Point(5, 6) -> "3", Point(5, 7) -> "7", Point(5, 8) -> "8", Point(5, 9) -> "2",
      Point(6, 1) -> "7", Point(6, 2) -> "8", Point(6, 3) -> "1", Point(6, 4) -> "2", Point(6, 5) -> "6", Point(6, 6) -> "9", Point(6, 7) -> "4", Point(6, 8) -> "3", Point(6, 9) -> "5",
      Point(7, 1) -> "1", Point(7, 2) -> "9", Point(7, 3) -> "7", Point(7, 4) -> "5", Point(7, 5) -> "8", Point(7, 6) -> "2", Point(7, 7) -> "3", Point(7, 8) -> "4", Point(7, 9) -> "6",
      Point(8, 1) -> "8", Point(8, 2) -> "5", Point(8, 3) -> "3", Point(8, 4) -> "4", Point(8, 5) -> "7", Point(8, 6) -> "6", Point(8, 7) -> "2", Point(8, 8) -> "9", Point(8, 9) -> "1",
      Point(9, 1) -> "2", Point(9, 2) -> "4", Point(9, 3) -> "6", Point(9, 4) -> "3", Point(9, 5) -> "9", Point(9, 6) -> "1", Point(9, 7) -> "5", Point(9, 8) -> "7", Point(9, 9) -> "8")
    assert(Sudoku.solutions(Sudoku(s)).contains(Sudoku(s)))
    assert(Sudoku.solutions(Sudoku(s.tail)).contains(Sudoku(s)))
    assert(Sudoku.solutions(Sudoku(s.drop(2))).contains(Sudoku(s)))
  }

  test("P98") {
    val points = Map(
      Point(1, 1) -> ".", Point(1, 2) -> "X", Point(1, 3) -> "X", Point(1, 4) -> "X", Point(1, 5) -> ".", Point(1, 6) -> ".", Point(1, 7) -> ".", Point(1, 8) -> ".",
      Point(2, 1) -> "X", Point(2, 2) -> "X", Point(2, 3) -> ".", Point(2, 4) -> "X", Point(2, 5) -> ".", Point(2, 6) -> ".", Point(2, 7) -> ".", Point(2, 8) -> ".",
      Point(3, 1) -> ".", Point(3, 2) -> "X", Point(3, 3) -> "X", Point(3, 4) -> "X", Point(3, 5) -> ".", Point(3, 6) -> ".", Point(3, 7) -> "X", Point(3, 8) -> "X",
      Point(4, 1) -> ".", Point(4, 2) -> ".", Point(4, 3) -> "X", Point(4, 4) -> "X", Point(4, 5) -> ".", Point(4, 6) -> ".", Point(4, 7) -> "X", Point(4, 8) -> "X",
      Point(5, 1) -> ".", Point(5, 2) -> ".", Point(5, 3) -> "X", Point(5, 4) -> "X", Point(5, 5) -> "X", Point(5, 6) -> "X", Point(5, 7) -> "X", Point(5, 8) -> "X",
      Point(6, 1) -> "X", Point(6, 2) -> ".", Point(6, 3) -> "X", Point(6, 4) -> "X", Point(6, 5) -> "X", Point(6, 6) -> "X", Point(6, 7) -> "X", Point(6, 8) -> ".",
      Point(7, 1) -> "X", Point(7, 2) -> "X", Point(7, 3) -> "X", Point(7, 4) -> "X", Point(7, 5) -> "X", Point(7, 6) -> "X", Point(7, 7) -> ".", Point(7, 8) -> ".",
      Point(8, 1) -> ".", Point(8, 2) -> ".", Point(8, 3) -> ".", Point(8, 4) -> ".", Point(8, 5) -> "X", Point(8, 6) -> ".", Point(8, 7) -> ".", Point(8, 8) -> ".",
      Point(9, 1) -> ".", Point(9, 2) -> ".", Point(9, 3) -> ".", Point(9, 4) -> "X", Point(9, 5) -> "X", Point(9, 6) -> ".", Point(9, 7) -> ".", Point(9, 8) -> ".")
    val n = Nonograms(List(List(3), List(2, 1), List(3, 2), List(2, 2), List(6), List(1, 5), List(6), List(1), List(2)),
      List(List(1, 2), List(3, 1), List(1, 5), List(7, 1), List(5), List(3), List(4), List(3)))
//    assert(n.genRows(List(3), 1).size == 6)
    n.apply(points)
    assert(n.getSolidRows == n.solidRows)
    assert(n.getSolidColumns == n.solidColumns)
    assert(n.isSolved)
    val solution = n.solutions.head
    assert(solution.getSolidRows == n.solidRows)
    assert(solution.getSolidColumns == n.solidColumns)
    assert(solution.isSolved)
    assert(solution.matrix == points)
  }
}
