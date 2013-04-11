package org.ghtorrent.netviz

import scala.util.control.Breaks._
import scala.collection.mutable.ArrayBuffer

case class Node[T](name: T, rank: Double = Graph.default_rank)

case class Link[T](source: Node[T], target: Node[T])

case class Graph[T](nodes: Seq[Node[T]], edges: Seq[Link[T]]) {

  private lazy val inEdgeNodes = {
    edges.foldLeft(Map[Node[T], List[Node[T]]]().withDefaultValue(List[Node[T]]())) {
      (acc, n) =>
        acc ++ Map(n.target -> (n.source :: acc(n.target)))
    }
  }

  private lazy val outEdgeNodes = {
    edges.foldLeft(Map[Node[T], List[Node[T]]]().withDefaultValue(List[Node[T]]())) {
      (acc, n) =>
        acc ++ Map(n.source -> (n.target :: acc(n.source)))
    }
  }

  def pagerank(deltaPR: Double = 0.0001, maxIterations: Int = 100, dumping: Double = 0.85): Seq[Node[T]] = {
    val nodesArr = nodes.toArray
    val nodeIdxs = (0 to (nodesArr.length - 1)).foldLeft(Map[Node[T], Int]()) {
      (acc, i) => acc ++ Map(nodesArr(i) -> i)
    }

    def iterHelper: Array[Double] = {
      var ranks = Array.fill[Double](nodes.length)(Graph.default_rank)

      var x = 0
      while (x < maxIterations) {
        System.out.println("Iteration " + x)

        val newRanks = Array.fill[Double](nodes.length)(0)

        var i = 0
        while (i < nodesArr.length) {
          val node = nodesArr(i)
          val incoming = inEdgeNodes(node)

          var j = 0
          var sumRanks = 0d
          while (j < incoming.length) {
            val node = incoming(j)
            val nodeIdx = nodeIdxs(node)
            val nodePR = if (nodeIdx >= i) ranks(nodeIdx) else newRanks(nodeIdx)
            val totalEdges = outEdgeNodes(node).length
            val PRincr = if (totalEdges == 0) 0 else (nodePR / totalEdges.toDouble)
            sumRanks += PRincr
            j += 1
          }

          val newRank = (1d - dumping) + dumping * sumRanks
          //System.out.println("Node " + node.name + " old=" + ranks(i) + " new=" + newRank)
          newRanks.update(i, newRank)
          i += 1
        }
        System.out.println("s=" + newRanks.foldLeft(0d) {
          (acc, x) => acc + x
        })

        var j = 0
        var found = false
        breakable {
          while (j < ranks.length) {
            if (Math.abs(ranks(j) - newRanks(j)) > deltaPR) {
              found = true
              break
            }
            j += 1
          }
        }

        if (found)
          ranks = newRanks.toArray
        else
          return ranks
        x += 1
      }
      ranks
    }

    return nodes.zip(iterHelper).map(x => x._1.copy[T](rank = x._2))
  }
}

object Graph {
  val default_rank = 1
}