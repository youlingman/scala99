/**
  * Created by Administrator on 2016-7-6.
  */
package graph {

  abstract class GraphBase[T, U] {

    val edgeSep: String
    val valueSep: String = "/"

    case class Edge(n1: Node, n2: Node, value: U) {
      def toTuple = (n1.value, n2.value, value)

      override def toString = value match {
        case () => n1.toString + n2.toString
        case v => n1.toString + edgeSep + n2.toString + valueSep + v.toString
      }
    }

    case class Node(value: T) {
      var adj: List[Edge] = Nil

      // neighbors are all nodes adjacent to this node.
      def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)

      def degree: Int = edges.count(e => e.n1 == this || e.n2 == this)

      def inDegree: Int = edges.count(e => e.n2 == this)

      def outDegree: Int = edges.count(e => e.n1 == this)
    }

    var nodes: Map[T, Node] = Map()
    var edges: List[Edge] = Nil

    // return a current type new instance
    def instance: GraphBase[T, U]

    // return a copy instance
    def copy: GraphBase[T, U]

    // If the edge E connects N to another node, returns the other node,
    // otherwise returns None.
    def edgeTarget(e: Edge, n: Node): Option[Node]

    // Get the edge between given two node n1 and n2.
    // otherwise returns None.
    def edgeBetween(n1: Node, n2: Node): Edge

    override def equals(o: Any) = o match {
      //      case g: GraphBase[T, U] => ((nodes.keySet -- g.nodes.keySet) ++ (g.nodes.keySet -- nodes.keySet)).isEmpty &&
      //        (edges.map(_.toTuple).toSet -- g.edges.map(_.toTuple).toSet).isEmpty
      case g: GraphBase[T, U] => (nodes.values.map(n => (n.value, n.adj.map(e => (edgeTarget(e, n).get.value, e.value)).toSet)).toSet -- g.nodes.values.map(n => (n.value, n.adj.map(e => (g.edgeTarget(e, n).get.value, e.value)).toSet)).toSet).isEmpty
      case _ => false
    }

    //  override def hashCode = edges.map(_.toTuple).toSet.hashCode()
    override def hashCode = nodes.values.map(n => (n.value, n.adj.map(e => (edgeTarget(e, n).get.value, e.value)).toSet)).toSet.hashCode()


    def addNode(value: T) = {
      val n = new Node(value)
      nodes = Map(value -> n) ++ nodes
      n
    }

    def addEdge(source: T, dest: T, value: U)

    def findPaths(from: T, to: T): List[List[T]] = {
      def _findPaths(curNode: Node, curList: List[T]): List[List[T]] = {
        if (curNode.value == to) List(curList)
        else curNode.neighbors.filter(n => !curList.contains(n.value)).flatMap(n => _findPaths(n, n.value :: curList))
      }
      _findPaths(nodes(from), List(from)).map(_.reverse)
    }

    def findCycles(from: T): List[List[T]] =
      nodes(from).neighbors.flatMap(n => findPaths(n.value, from)).filter(_.length > 2).map(from :: _)

    def spanningTrees: List[GraphBase[T, U]] = {
      def _spanningTrees(curNodes: List[Node], curGraph: GraphBase[T, U]): List[GraphBase[T, U]] = {
        if (curGraph.nodes.size == nodes.size) {
          List(curGraph)
        }
        else curNodes.flatMap(curNode => curNode.neighbors.filter(n => !curGraph.nodes.contains(n.value)).flatMap(n => {
          val g = curGraph.copy
          g.addNode(n.value)
          val e = edgeBetween(curNode, n)
          g.addEdge(e.n1.value, e.n2.value, e.value)
          _spanningTrees(n :: curNodes, g)
        }))
      }
      nodes.flatMap(n => {
        val g = instance
        g.addNode(n._1)
        _spanningTrees(List(n._2), g)
      }).toList.distinct
    }

    def isTree: Boolean = spanningTrees.size == 1

    def isConnected: Boolean = spanningTrees.nonEmpty

    def minimalSpanningTree: GraphBase[T, U] = spanningTrees.sortBy(g => g.edges.foldLeft(0) { (n, g) => n + g.value.toString.toInt }).head

    def isIsomorphicTo(g: GraphBase[T, U]): Boolean = {
      def listMappings(tNodes: List[Node], oNodes: List[g.Node]) =
        for (t <- tNodes; o <- oNodes) yield (t, o)

      // Used on partially-filled isomorphisms to weed out some early.
      def isValidMapping(mapping: Map[Node, g.Node]): Boolean =
        nodes.values forall { n =>
          !mapping.contains(n) ||
            n.neighbors.filter(mapping.contains).forall(tnn => mapping(n).neighbors.contains(mapping(tnn)))
        }

      def _isIsomorphicTo(ns: List[Node], nns: List[g.Node], mapping: Map[Node, g.Node]): Boolean =
        if (ns.isEmpty) {
          nodes.values.forall( n => Set(n.neighbors.map(mapping.apply(_)): _*) == Set(mapping(n).neighbors: _*) )
        }
        else {
          listMappings(ns, nns).filter(p => isValidMapping(mapping + p)).exists( p => _isIsomorphicTo(ns.filterNot(_ == p._1), nns.filterNot(_ == p._2), mapping + p) )
        }
      _isIsomorphicTo(nodes.values.toList, g.nodes.values.toList, Map())
    }

    def nodesByDegree: List[Node] = nodes.values.toList.sortBy(_.degree).reverse

    def colorNodes: List[(Node, Int)] = {
      def _colorNodes(curNodes: List[Node], curList: List[(Node, Int)]): List[(Node, Int)] =
        if (curNodes.isEmpty) curList
        else _colorNodes(curNodes.tail, (curNodes.head, Stream.from(1).filter(i => !curList.filter(n => curNodes.head.neighbors.contains(n._1)).map(_._2).distinct.contains(i)).head) :: curList)
      _colorNodes(nodes.values.toList.sortBy(_.inDegree).reverse, List())
    }

    def nodesByDepthFrom(from: T): List[Node] = {
      def _nodesByDepthFrom(curNodes: List[Node], curList: List[Node]): List[Node] = {
        if (curNodes.isEmpty) curList
        else _nodesByDepthFrom(curNodes.tail ::: curNodes.head.neighbors.filter(n => !curList.contains(n) && !curNodes.contains(n)), curNodes.head :: curList)
      }
      _nodesByDepthFrom(List(nodes.get(from).get), List()).reverse
    }

    def splitGraph: List[GraphBase[T, U]] = {
      def _splitGraph(curNodes: List[Node]): List[GraphBase[T, U]] = {
        if (curNodes.isEmpty) List()
        else {
          println("---current node" + curNodes.head)
          val curCluster = nodesByDepthFrom(curNodes.head.value).map(v => nodes.get(v.value).get)
          val adjForm = curCluster.map(n => (n.value, n.adj.map(e => (edgeTarget(e, n).get.value, e.value))))
          this match {
            case _: Graph[_, _] => Graph.adjacentLabel(adjForm) :: _splitGraph(curNodes.filter(n => !curCluster.contains(n)))
            case _: Digraph[_, _] => Digraph.adjacentLabel(adjForm) :: _splitGraph(curNodes.filter(n => !curCluster.contains(n)))
          }
        }
      }
      _splitGraph(nodes.values.toList)
    }

    def isBipartite: Boolean = colorNodes.map(_._2).distinct.size == 2

  }

  // 无向图
  class Graph[T, U] extends GraphBase[T, U] {
    override def equals(o: Any) = o match {
      case g: Graph[_, _] => super.equals(g)
      case _ => false
    }

    override def instance: Graph[T, U] = new Graph[T, U]

    override def copy: Graph[T, U] = {
      val g = new Graph[T, U]
      g.nodes = Map()
      nodes.foreach(n => g.addNode(n._1))
      g.edges = Nil
      edges.foreach(e => g.addEdge(e.toTuple._1, e.toTuple._2, e.toTuple._3))
      g
    }

    val edgeSep: String = "-"

    def edgeTarget(e: Edge, n: Node): Option[Node] =
      if (e.n1 == n) Some(e.n2)
      else if (e.n2 == n) Some(e.n1)
      else None

    def edgeBetween(n1: Node, n2: Node): Edge =
      edges.filter(e => (e.n1 == n1 && e.n2 == n2) || (e.n1 == n2 && e.n2 == n1)).head

    def addEdge(n1: T, n2: T, value: U) = {
      val e = new Edge(nodes(n1), nodes(n2), value)
      edges = e :: edges
      nodes(n1).adj = e :: nodes(n1).adj
      nodes(n2).adj = e :: nodes(n2).adj
    }

    def toTermForm: (List[T], List[(T, T, U)]) = (nodes.keys.toList, edges.map(_.toTuple))
  }

  // 有向图
  class Digraph[T, U] extends GraphBase[T, U] {
    override def equals(o: Any) = o match {
      case g: Digraph[_, _] => super.equals(g)
      case _ => false
    }

    override def instance: Digraph[T, U] = new Digraph[T, U]

    override def copy: Digraph[T, U] = {
      val g = new Digraph[T, U]
      g.nodes = Map()
      nodes.foreach(n => g.addNode(n._1))
      g.edges = Nil
      edges.foreach(e => g.addEdge(e.toTuple._1, e.toTuple._2, e.toTuple._3))
      g
    }

    val edgeSep: String = ">"

    def edgeTarget(e: Edge, n: Node): Option[Node] =
      if (e.n1 == n) Some(e.n2)
      else None

    def edgeBetween(n1: Node, n2: Node): Edge =
      edges.filter(e => e.n1 == n1 && e.n2 == n2).head

    def addEdge(source: T, dest: T, value: U) = {
      val e = new Edge(nodes(source), nodes(dest), value)
      edges = e :: edges
      nodes(source).adj = e :: nodes(source).adj
    }

    def toAdjacentForm: List[(T, List[(T, U)])] = nodes.values map { n => (n.value, n.adj map { e => (edgeTarget(e, n).get.value, e.value) }) } toList
  }

  abstract class GraphObjBase {
    type GraphClass[T, U]

    def addLabel[T](edges: List[(T, T)]) =
      edges.map(v => (v._1, v._2, ()))

    def term[T](nodes: List[T], edges: List[(T, T)]) =
      termLabel(nodes, addLabel(edges))

    def termLabel[T, U](nodes: List[T], edges: List[(T, T, U)]): GraphClass[T, U]

    def addAdjacentLabel[T](nodes: List[(T, List[T])]) =
      nodes.map(a => (a._1, a._2.map((_, ()))))

    def adjacent[T](nodes: List[(T, List[T])]) =
      adjacentLabel(addAdjacentLabel(nodes))

    def adjacentLabel[T, U](nodes: List[(T, List[(T, U)])]): GraphClass[T, U]

    def genGraph(regex: String, str: String): (List[String], List[(String, String, Any)]) = {
      var nodes = List[String]()
      var edges = List[(String, String, Any)]()
      regex.r.findAllMatchIn(str) foreach {
        a =>
          val na = a.group(1)
          val nb = a.group(2)
          val value = a.group(3) match {
            case null => ()
            case n => n
          }
          if (!nodes.contains(na) && null != na) nodes ::= na
          if (!nodes.contains(nb) && null != nb) nodes ::= nb
          if (null != na && null != nb) edges ::= ((na, nb, value))
      }
      (nodes, edges)
    }
  }

  object Graph extends GraphObjBase {
    type GraphClass[T, U] = Graph[T, U]

    def apply[T](value: T) = new Graph()

    def termLabel[T, U](nodes: List[T], edges: List[(T, T, U)]) = {
      val g = new Graph[T, U]
      nodes.map(g.addNode)
      edges.map(v => g.addEdge(v._1, v._2, v._3))
      g
    }

    def adjacentLabel[T, U](nodes: List[(T, List[(T, U)])]) = {
      val g = new Graph[T, U]
      for ((v, a) <- nodes) g.addNode(v)
      for ((n1, a) <- nodes; (n2, l) <- a) {
        if (!g.nodes(n1).neighbors.contains(g.nodes(n2)))
          g.addEdge(n1, n2, l)
      }
      g
    }

    def fromString(str: String) = {
      val regex = "([a-zA-Z0-9])-?([a-zA-Z0-9])?/?([a-zA-Z0-9])?"
      genGraph(regex, str) match {
        case (nodes, edges) =>
          //          termLabel(nodes, edges.groupBy(e => List(e._1, e._2).sorted).values.map(_.head).toList)
          termLabel(nodes, edges)
      }
    }
  }

  object Digraph extends GraphObjBase {
    type GraphClass[T, U] = Digraph[T, U]

    def termLabel[T, U](nodes: List[T], edges: List[(T, T, U)]) = {
      val g = new Digraph[T, U]
      nodes.map(g.addNode)
      edges.map(v => g.addEdge(v._1, v._2, v._3))
      g
    }

    def adjacentLabel[T, U](nodes: List[(T, List[(T, U)])]) = {
      val g = new Digraph[T, U]
      for ((n, a) <- nodes) g.addNode(n)
      for ((s, a) <- nodes; (d, l) <- a) g.addEdge(s, d, l)
      g
    }

    def fromStringLabel[T, U](str: String) = {
      val regex = "([a-zA-Z0-9])>?([a-zA-Z0-9])?/?([a-zA-Z0-9])?"
      genGraph(regex, str) match {
        case (nodes, edges) =>
          termLabel(nodes, edges)
      }
    }
  }

}