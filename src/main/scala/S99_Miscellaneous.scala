import graph.Graph

import scala.collection.immutable.SortedMap
import scala.collection.immutable.Stream.Empty

/**
  *
  * Created by Administrator on 2017-1-10.
  */
object S99_Miscellaneous {

  /*
    P90 (**) Eight queens problem
   */
  def eightQueens(n: Int): List[List[Int]] = {
    def _eightQueens(curColumn: Int, curList: List[Int]): List[List[Int]] =
      if (curList.size == n) List(curList.map(_ + 1))
      else (0 until n).filter(n => curList.zipWithIndex.forall(p => n != p._1 && Math.abs(p._1 - n) != Math.abs(p._2 - curColumn))).flatMap(n => _eightQueens(curColumn + 1, curList ::: List(n))).toList
    _eightQueens(0, List())
  }

  case class Point(x: Int, y: Int) {
    def +(p: Point): Point = Point(x + p.x, y + p.y)
  }

  /*
    P91 (**) Knight's tour.
   */
  class KnightTour(n: Int, from: Point) {
    val steps = List(Point(1, 2), Point(1, -2), Point(-1, 2), Point(-1, -2), Point(2, 1), Point(2, -1), Point(-2, 1), Point(-2, -1))

    def nextSteps(from: Point): Stream[Point] = steps.map(p => from + p).filter(validCoordinator).toStream

    def validCoordinator(p: Point) = p.x >= 0 && p.y >= 0 && p.x < n && p.y < n

    def solutions: Stream[List[Point]] = {
      def _knightTour(curLoc: Point, curPath: List[Point]): Stream[List[Point]] =
        if (curPath.size == n * n) Stream((curLoc :: curPath).reverse)
        else nextSteps(curLoc).filter(!curPath.contains(_)).flatMap(p => _knightTour(p, p :: curPath))
      _knightTour(from, List(from))
    }

    lazy val closedTours = solutions.filter(_.last == from)
  }

  //  def knightTour(n: Int, from: Point): Stream[List[Point]] = {
  //    val steps = List(Point(1, 2), Point(1, -2), Point(-1, 2), Point(-1, -2), Point(2, 1), Point(2, -1), Point(-2, 1), Point(-2, -1))
  //    def nextSteps(from: Point): Stream[Point] = steps.map(p => from + p).filter(validCoordinator).toStream
  //
  //    def validCoordinator(p: Point) = p.x >= 0 && p.y >= 0 && p.x < n && p.y < n
  //
  //    def _knightTour(curLoc: Point, curPath: List[Point]): Stream[List[Point]] =
  //      if (curLoc == from && curPath.size == n * n) Stream(curPath.reverse)
  //      else {
  //        //        println(curPath)
  //        nextSteps(curLoc).filter(!curPath.contains(_)).flatMap(p => _knightTour(p, p :: curPath))
  //      }
  //    _knightTour(from, List())
  //  }

  /*
    P92 (***) Von Koch's conjecture.
   */
  def vonKochConjecture(tree: Graph[String, Any]): Boolean =
    tree.isTree && (1 to tree.nodes.size).permutations. // get permutations for nodes array
      map(_.zip(tree.nodes.keys).map(p => (p._2, p._1)).toMap). // mapping permutations, make num tag as key
      map(p => tree.edges.map(e => Math.abs(p.get(e.n1.value).get - p.get(e.n2.value).get))). // get the diff edges for each permutation mapping
      exists(_.toSet == (1 until tree.nodes.size).toSet) // judge if valid diff edges exist

  /*
    P93 (***) An arithmetic puzzle.
   */
  def arithmeticPuzzle(nums: List[Int]): List[String] = {
    // mapper part
    val ops = Map("+" -> Math.addExact _, "-" -> Math.subtractExact _, "*" -> Math.multiplyExact _, "/" -> Math.floorDiv _)

    def genExprs(ns: List[Int]): List[(Int, String)] =
      if (ns.size == 1) List((ns.head, ns.head.toString))
      else (1 until ns.size).map(ns.splitAt).flatMap(p => combineExpr(genExprs(p._1), genExprs(p._2))).toList

    def combineExpr(lefts: List[(Int, String)], rights: List[(Int, String)]): List[(Int, String)] = {
      for (l <- lefts; r <- rights; op <- ops; if (op._1 != "/") || (op._1 == "/" && r._1 != 0 && l._1 % r._1 == 0)) yield (op._2(l._1, r._1), "(" + l._2 + op._1 + r._2 + ")")
    }

    // reducer part
    def filterExpr(lefts: List[(Int, String)], rights: List[(Int, String)]): List[String] = {
      val lMap = lefts.groupBy(_._1)
      val rMap = rights.groupBy(_._1)
      lMap.filterKeys(rMap.contains).flatMap(p => for (l <- p._2; r <- rMap.get(p._1).get) yield l._2 + "=" + r._2).toList
    }

    (1 until nums.size).map(nums.splitAt).flatMap(p => filterExpr(genExprs(p._1), genExprs(p._2))).toList
  }

  def kRegularGraph(N: Int, K: Int): List[Graph[Int, Any]] = ???

  def englishNumber(num: Int): String = ???

  /*
    P96 (**) Syntax checker.
   */
  def isIdentifier(str: String): Boolean = "[a-zA-Z](_?[a-zA-Z0-9])*".r.pattern.matcher(str).matches()

  /*
    P97 Sudoku
   */
  class Sudoku {
    val row: Int = 9
    val column: Int = 9
    var matrix = genMatrix(9, 9)

    def apply(points: Map[Point, String]) = {
      matrix = matrix.map(p => if (points.contains(p._1)) (p._1, points(p._1)) else p)
      //      matrix = points
    }

    override def equals(o: Any) = o match {
      case s: Sudoku => matrix.equals(s.matrix)
      case _ => false
    }

    override def hashCode = matrix.hashCode

    override def toString = (1 to 9).map(row => matrix.toList.filter(_._1.x == row)).flatMap(p => p.sortBy(_._1.y).map(_._2).mkString.concat("\n")).mkString

    def validPoint(point: (Point, String)): Boolean = validRow(point) && validColumn(point) && validSquare(point)

    def validRow(point: (Point, String)): Boolean = !matrix.filter(p => p._1 != point._1 && p._1.x == point._1.x).values.exists(_ == point._2)

    def validColumn(point: (Point, String)): Boolean = !matrix.filter(p => p._1 != point._1 && p._1.y == point._1.y).values.exists(_ == point._2)

    def validSquare(point: (Point, String)): Boolean = !matrix.filter(p => p._1 != point._1 && Sudoku.sameSquare(point._1, p._1)).values.exists(_ == point._2)


    lazy val solution = Sudoku.solutions(this).headOption
  }

  object Sudoku {
    def apply(points: Map[Point, String]) = {
      val s = new Sudoku
      s.apply(points)
      s
    }

    def sameSquare(p: Point, pp: Point): Boolean = (p.x - 1) / 3 == (pp.x - 1) / 3 && (p.y - 1) / 3 == (pp.y - 1) / 3

    def step(o: Sudoku, point: (Point, String)) = Sudoku(o.matrix.map(p => if (p._1 == point._1) point else p))

    def solutions(s: Sudoku): Stream[Sudoku] = s.matrix.filter(_._2 == ".").toStream.map(_._1) match {
      case Empty => Stream(s)
      case holes => holes.flatMap(p => (1 to 9).map(n => (p, n.toString))).filter(s.validPoint).map(p => step(s, p)).flatMap(solutions)
    }

    def solve(s: Sudoku): List[Sudoku] = s.matrix.filter(_._2 == ".").toList.map(_._1) match {
      case List() => List(s)
      case holes => holes.flatMap(p => (1 to 9).map(n => (p, n.toString))).filter(s.validPoint).map(p => step(s, p)).flatMap(solve)
    }
  }

  /*
    P98 Nonograms
  */
  class Nonograms(val solidRows: List[List[Int]], val solidColumns: List[List[Int]]) {
    val row = solidRows.size
    val column = solidColumns.size
    var matrix: Map[Point, String] = genMatrix(row, column)

    def apply(points: Map[Point, String]) = {
      matrix = matrix.map(p => if (points.contains(p._1)) (p._1, points(p._1)) else p)
      this
    }

    def isSolved: Boolean = getSolidRows == solidRows && getSolidColumns == solidColumns

    def getSolidRows: List[List[Int]] = SortedMap(matrix.groupBy(_._1.x).toSeq: _*).values.map(_.toSeq.sortBy(_._1.y)).map(l => l :+(Point(0, 0), "")).
      map(_.foldLeft((0, List()): (Int, List[Int])) {
        case (p, (_, "X")) => (p._1 + 1, p._2)
        case ((0, l), _) => (0, l)
        case ((n, l), _) => (0, n :: l)
      }).toList.map(_._2.reverse)

    def getSolidColumns: List[List[Int]] = SortedMap(matrix.groupBy(_._1.y).toSeq: _*).values.map(_.toSeq.sortBy(_._1.x)).map(l => l :+(Point(0, 0), "")).
      map(_.foldLeft((0, List()): (Int, List[Int])) {
        case (p, (_, "X")) => (p._1 + 1, p._2)
        case ((0, l), _) => (0, l)
        case ((n, l), _) => (0, n :: l)
      }).toList.map(_._2.reverse)

    // todo 需要从每一行solidRow生成所有可能的行，然后对所有可能的行merge成可能的总的集合（笛卡尔积？），最后再对这个总的集合作filter得到最终解
    // todo 从solidRow和columnSize得到可能的combinations: List[List[Int]]，然后从rowIndex和columnSize可以得到该行对应的模板rowTemp: Map[Point, String]，
    // todo 最后再从combinations填入rowTemp内

    def gen
    def genRows(solidRow: List[Int], columnSize: Int, rowIndex: Int): List[Map[Point, String]] = ???

    def genNonograms(rows: List[List[Map[Point, String]]]): List[Map[Point, String]] = rows match {
      case Nil => List()
      case h :: t => h.flatMap(i => genNonograms(t).map(i ++ _))
    }

    def solutions: List[Nonograms] = genNonograms(solidRows.zip(List.range(1, row + 1)).map(n => genRows(n._1, column, n._2))).map(Nonograms.fork(this, _))
  }

  object Nonograms {
    def apply(solidRows: List[List[Int]], solidColumns: List[List[Int]]) = {
      val n = new Nonograms(solidRows, solidColumns)
      n
    }

    def fork(n: Nonograms, matrix: Map[Point, String]): Nonograms = Nonograms(n.solidRows, n.solidColumns).apply(matrix)

    def genRow(solidRow: List[Int]): List[List[String]] = ???
  }

  /*
    P99 crosswordPuzzle
  */
  def crosswordPuzzle = ???

  def genMatrix(n: Int, m: Int): Map[Point, String] = {
    var mapping = Map[Point, String]()
    for (i <- 1 to n; j <- 1 to m) mapping += (Point(i, j) -> ".")
    mapping
  }
}
