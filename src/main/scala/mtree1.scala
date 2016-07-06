/**
  * Created by Administrator on 2016-6-28.
  */
package multiwaytree {

  case class MTree[+T](value: T, children: List[MTree[T]]) {
    def this(value: T) = this(value, List())

    //    override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"

    override def toString = value.toString + children.foldLeft("") { (s, m) => s + m.toString } + "^"

    def nodeCount: Int = children.foldLeft(1) { _ + _.nodeCount }

    def internalPathLength: Int = children.foldLeft(0) { (c, n) => c + n.nodeCount + n.internalPathLength }

    //    def postorder: List[T] = children.foldLeft(List[T]()) { (l, n) => l ::: n.postorder } ::: List(value)
    def postorder: List[T] = children.flatMap(_.postorder) ::: List(value)

    def lispyTree: String = this match {
      case MTree(_, List()) => value.toString
      case _ => "(" + value.toString + children.foldLeft("") { _ + " " + _.lispyTree } + ")"
    }
  }

  object MTree {
    def apply[T](value: T) = new MTree(value, List())

    // copy from solution
    implicit def string2MTree(s: String): MTree[Char] = {
      def nextStrBound(pos: Int, nesting: Int): Int =
        if (nesting == 0) pos
        else nextStrBound(pos + 1, if (s(pos) == '^') nesting - 1 else nesting + 1)
      def splitChildStrings(pos: Int): List[String] =
        if (pos >= s.length - 1) Nil
        else {
          val end = nextStrBound(pos + 1, 1)
          s.substring(pos, end - 1) :: splitChildStrings(end)
        }
      MTree(s(0), splitChildStrings(1).map(string2MTree(_)))
    }
  }

}
