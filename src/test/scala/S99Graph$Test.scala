import org.scalatest.FunSuite
import graph.Graph
import graph.Digraph

/**
  * Created by Administrator on 2016-10-9.
  */
class S99Graph$Test extends FunSuite {
  test("P80") {
    assert((Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").toTermForm._1.toSet -- List("d", "k", "h", "c", "f", "g", "b").toSet) isEmpty)
    assert((Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").toTermForm._2.toSet -- List(("h", "g", ()), ("k", "f", ()), ("f", "b", ()), ("g", "h", ()), ("f", "c", ()), ("b", "c", ())).toSet) isEmpty)
    assert((Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").toAdjacentForm.toMap.keySet -- List(("m", List(("q", "7"))), ("p", List(("m", "5"), ("q", "9"))), ("k", List()), ("q", List())).toMap.keySet) isEmpty)
    assert((Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").toAdjacentForm.toMap.values.map(_.toSet).toSet -- List(("m", List(("q", "7"))), ("p", List(("m", "5"), ("q", "9"))), ("k", List()), ("q", List())).toMap.values.map(_.toSet).toSet) isEmpty)
  }

  test("P81") {
    assert(Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").findPaths("b", "k").toSet == List(List("b", "c", "f", "k"), List("b", "f", "k")).toSet)
    assert(Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "q").toSet == List(List("p", "q"), List("p", "m", "q")).toSet)
  }

  test("P82") {
    assert(Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").findCycles("f").toSet == List(List("f", "c", "b", "f"), List("f", "b", "c", "f")).toSet)
  }

  test("P83") {
    assert(Graph.fromString("[a-b, b-c]").spanningTrees.size == 1)
    assert(Graph.fromString("[a-b, b-c]").isConnected)
    assert(Graph.fromString("[a-b, b-c]").isTree)
    assert(Graph.fromString("[a-b, b-c, a-c]").spanningTrees.size == 3)
    assert(Graph.fromString("[a-b, b-c, a-c]").isConnected)
    assert(!Graph.fromString("[a-b, b-c, a-c]").isTree)
    assert(!Graph.fromString("[a-b, b-c, d]").isConnected)
    assert(!Graph.fromString("[a-b, b-c, d]").isTree)
    assert(Digraph.fromStringLabel("[p>q/9, p>m/5]").isConnected)
    assert(Digraph.fromStringLabel("[p>q/9, p>m/5]").isTree)
    assert(Digraph.fromStringLabel("[p>q/9, q>m/5, m>p/4]").isConnected)
    assert(!Digraph.fromStringLabel("[p>q/9, q>m/5, m>p/4]").isTree)
    assert(Digraph.fromStringLabel("[p>q/9, q>m/5, m>p/4]").spanningTrees.size == 3)
    assert(!Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").isConnected)
    assert(!Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").isTree)
  }

  test("P84") {
    assert(Graph.fromString("[a-b/1, b-c/2, a-c/3]").minimalSpanningTree.equals(Graph.fromString("[a-b/1, b-c/2]")))
    assert(Digraph.fromStringLabel("[p>q/9, q>m/5, m>p/4]").minimalSpanningTree.equals(Digraph.fromStringLabel("[q>m/5, m>p/4]")))
  }

  test("P85") {
    assert(Graph.fromString("[a-b]").isIsomorphicTo(Graph.fromString("[5-7]")))
    assert(Graph.fromString("[a-b, b-c, c-a]").isIsomorphicTo(Graph.fromString("[1-2, 2-3, 3-1]")))
    assert(!Graph.fromString("[a-b, b-c]").isIsomorphicTo(Graph.fromString("[1-2, 2-3, 3-1]")))
    assert(Digraph.fromStringLabel("[p>q/9, q>m/5, k>m/6]").isIsomorphicTo(Digraph.fromStringLabel("[4>3/1, 2>3/2, 1>2/5]")))
    assert(!Digraph.fromStringLabel("[p>q/9, q>m/5, k>d/6]").isIsomorphicTo(Digraph.fromStringLabel("[4>3/1, 2>3/2, 1>2/5]")))
  }

  test("P86") {
    assert(Graph.fromString("[a-b, b-c, a-c, a-d]").nodes("a").degree == 3)
    assert(Graph.fromString("[a-b, b-c, a-c, a-d]").nodes("b").degree == 2)
    assert(Graph.fromString("[a-b, b-c, a-c, a-d]").nodes("c").degree == 2)
    assert(Graph.fromString("[a-b, b-c, a-c, a-d]").nodes("d").degree == 1)
    assert(Graph.fromString("[a-b, b-c, a-c, a-d]").nodesByDegree.map(_.value) == List("a", "c", "b", "d"))
    assert(Digraph.fromStringLabel("[a>b, c>a, d>b]").nodes("c").degree == 1)
    assert(Digraph.fromStringLabel("[a>b, c>a, d>b]").nodes("a").degree == 2)
    assert(Digraph.fromStringLabel("[a>b, c>a, d>b]").nodes("d").degree == 1)
    assert(Digraph.fromStringLabel("[a>b, c>a, d>b]").nodes("b").degree == 2)
  }

  test("P87") {
    assert(Graph.fromString("[a-b, b-c, a-c, a-d]").nodesByDepthFrom("d").map(_.value) == List("d", "a", "c", "b") || Graph.fromString("[a-b, b-c, a-c, a-d]").nodesByDepthFrom("d").map(_.value) == List("d", "a", "b", "c"))
    assert(Digraph.fromStringLabel("[p>q/9, q>m/5, m>p/4]").nodesByDepthFrom("p").map(_.value) == List("p", "q", "m"))
    assert(Digraph.fromStringLabel("[p>q/9, q>m/5, m>p/4]").nodesByDepthFrom("q").map(_.value) == List("q", "m", "p"))
    assert(Digraph.fromStringLabel("[p>q/9, q>m/5, m>p/4]").nodesByDepthFrom("m").map(_.value) == List("m", "p", "q"))
    assert(Digraph.fromStringLabel("[p>q/9, q>m/5, m>t/4]").nodesByDepthFrom("q").map(_.value) == List("q", "m", "t"))
  }

  test("P88") {
    assert(Graph.fromString("[a-b, c]").splitGraph == List(Graph.fromString("[a-b]"), Graph.fromString("c")))
    assert(Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").splitGraph == List(Graph.fromString("[b-c, f-c, f-b, k-f]"), Graph.fromString("[g-h, h-g]"), Graph.fromString("[d]")))
  }

  test("P89") {
    assert(Digraph.fromStringLabel("[a>b, c>a, d>b]").isBipartite)
    assert(!Graph.fromString("[a-b, b-c, c-a]").isBipartite)
    assert(Graph.fromString("[a-b, b-c, d]").isBipartite)
    assert(!Graph.fromString("[a-b, b-c, d, e-f, f-g, g-e, h]").isBipartite)
  }
}
