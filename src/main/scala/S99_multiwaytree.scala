import multiwaytree.MTree

/**
  * Created by Administrator on 2016-6-28.
  */
object S99_multiwaytree {
  // my solution
  def string2MTree(str: String): MTree[Char] = {
    def string2MTreeR(pos: Int): (List[MTree[Char]], Int) =
      if (pos >= str.length) (Nil, pos)
      else
        str.charAt(pos) match {
          case '^' => (Nil, pos + 1)
          case c =>
            var list = List[MTree[Char]]()
            var (cList, p) = string2MTreeR(pos + 1)
            while (cList != Nil) {
              list = list ::: cList
              val n = string2MTreeR(p)
              cList = n._1
              p = n._2
            }
            (List(MTree(str.charAt(pos), list)), p)
        }
    string2MTreeR(0)._1.head
  }

}
