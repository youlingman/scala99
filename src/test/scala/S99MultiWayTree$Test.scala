import multiwaytree.MTree
import org.scalatest.FunSuite

/**
  * Created by Administrator on 2016-6-29.
  */
class S99MultiWayTree$Test extends FunSuite {
  test("P70C") {
    assert(MTree('a', List(MTree('f'))).nodeCount == 2)
    assert(MTree('a', Nil).nodeCount == 1)
  }

  test("P70") {
    assert(S99_multiwaytree.string2MTree("a^") == MTree('a', List()))
    assert(S99_multiwaytree.string2MTree("afg^^c^bd^e^^^") == MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))))
    assert(MTree.string2MTree("a^") == MTree('a', List()))
    assert(MTree.string2MTree("afg^^c^bd^e^^^") == MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))))
  }

  test("P71") {
    assert(MTree('a').internalPathLength == 0)
    assert(MTree('b', List(MTree('d'), MTree('e'))).internalPathLength == 2)
    assert(MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).internalPathLength == 9)
  }

  test("P72") {
    assert(MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).postorder == List('g', 'f', 'c', 'd', 'e', 'b', 'a'))
    assert(MTree('b', List(MTree('d'), MTree('e'))).postorder == List('d', 'e', 'b'))
    assert(MTree('f', List(MTree('g'))).postorder == List('g', 'f'))
    assert(MTree('a').postorder == List('a'))
  }

  test("P73") {
    assert(MTree("a", List()).lispyTree == "a")
    assert(MTree("a", List(MTree("b"))).lispyTree == "(a b)")
    assert(MTree("a", List(MTree("b", List(MTree("c"))))).lispyTree == "(a (b c))")
    assert(MTree('b', List(MTree('d'), MTree('e'))).lispyTree == "(b d e)")
    assert(MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).lispyTree == "(a (f g) c (b d e))")
  }
}
