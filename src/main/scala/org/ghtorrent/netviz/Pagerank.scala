package org.ghtorrent.netviz

case class RankedNode[T](name: T, rank: Double = Network.default_rank)
case class Link[T](source: RankedNode[T], target: RankedNode[T])

case class Network[T](nodes: Seq[RankedNode[T]], edges: Seq[Link[T]]) {

  //def pagerank : Seq[RankedNode[T]] = converge(nodes).sortWith((a,b) => if(a.rank > b.rank) true else false)

  private def outEdges(node: RankedNode[T])  = edges.filter(e => e.source == node)
  private def inEdges(node: RankedNode[T])   = edges.filter(e => e.target == node)

  def pagerank(deltaPR: Double = 0.0001, maxIterations: Int = 100, dumping: Double = 0.85) : Seq[RankedNode[T]] = {
    val nodesArr = nodes.toArray
    val nodeIdxs = (0 to (nodesArr.size - 1)).foldLeft(Map[RankedNode[T], Int]()){(acc, i) => acc ++ Map(nodesArr(i) -> i)}

    def iterHelper : Array[Double] = {
      var ranks = Array.fill[Double](nodes.size)(Network.default_rank)
      (1 to maxIterations).foreach { x =>
        //System.out.println("Iteration " + x)

        val newRanks = Array.fill[Double](nodes.size)(0)

        for (i <- 0 to (nodesArr.size - 1)) {
          val node = nodesArr(i)
          val incoming = inEdges(node).map {x => x.source}

          val sumRanks = incoming.foldLeft(0d) {
            (acc, a) =>
              val nodeIdx = nodeIdxs(a)
              val nodePR = if (nodeIdx >= i) ranks(nodeIdx) else newRanks(nodeIdx)
              val totalEdges = outEdges(a).size
              val PRincr = if (totalEdges == 0) 0 else (nodePR / totalEdges.toDouble)
              acc + PRincr
          }

          val newRank = (1d - dumping) + dumping * sumRanks
          //System.out.println("Node " + node.name + " old=" + ranks(i) + " new=" + newRank)
          newRanks.update(i, newRank)
        }
        System.out.println("s=" + newRanks.foldLeft(0d){(acc,x) => acc + x})
        ranks.zip(newRanks).find(a => Math.abs(a._1 - a._2) > deltaPR) match {
          case Some(x) => ranks = newRanks.toArray
          case None => return ranks
        }
      }
      ranks
    }
    return nodes.zip(iterHelper).map(x => x._1.copy[T](rank = x._2))
  }
}

object Network {
  val default_rank = 1
}