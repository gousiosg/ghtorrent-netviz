package org.ghtorrent.netviz

import scala.util.control.Breaks._
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.immutable.ParSeq

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

  def parPagerank(deltaPR: Double = 0.0001, maxIterations: Int = 100, dumping: Double = 0.85): ParSeq[Node[T]] = {
    val nodeIdxs = (0 to (nodes.length - 1)).foldLeft(Map[Node[T], Int]()) {
      (acc, i) => acc ++ Map(nodes(i) -> i)
    }

    def iterHelper: Array[Double] = {
      var ranks = Array.fill[Double](nodes.length)(Graph.default_rank)
      val parNodes = nodes.par

      (1 to maxIterations).foreach {
        x =>
          System.out.println("Iteration " + x)
          val newRanks = parNodes.map {
            node =>
              val incoming = inEdgeNodes(node)

              val sumRanks = incoming.foldLeft(0d) {
                (acc, a) =>
                  val nodeIdx = nodeIdxs(a)
                  val nodePR = ranks(nodeIdx)
                  val totalEdges = outEdgeNodes(a).size
                  val PRincr = if (totalEdges == 0) 0 else (nodePR / totalEdges.toDouble)
                  acc + PRincr
              }
              val newRank = (1d - dumping) + dumping * sumRanks
              //System.out.println("Node " + node.name + " old=" + ranks(nodeIdxs(node)) + " new=" + newRank)
              newRank
          }.toArray

          System.out.println("s=" + newRanks.foldLeft(0d) {
            (acc, x) => acc + x
          })

          ranks.zip(newRanks).find(a => Math.abs(a._1 - a._2) > deltaPR) match {
            case Some(x) => ranks = newRanks
            case None => return ranks
          }
      }
      ranks
    }

    return nodes.zip(iterHelper).map(x => x._1.copy[T](rank = x._2)).toList.par
  }
}

object Graph {
  val default_rank = 1
}