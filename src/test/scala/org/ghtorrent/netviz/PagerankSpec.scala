package org.ghtorrent.netviz

import org.specs2.mutable._

class PagerankSpec extends Specification {

  val nodes1 = List(
    Node("A"),
    Node("B"),
    Node("C"),
    Node("D")
  )

  val links1 = List(
    Edge(nodes1(0), nodes1(1)),
    Edge(nodes1(1), nodes1(2)),
    Edge(nodes1(0), nodes1(2)),
    Edge(nodes1(2), nodes1(0)),
    Edge(nodes1(3), nodes1(2))
  )

  val graph1 = Graph(nodes1, links1)
  val prResult1 = List(
    Node("A", 1.490),
    Node("B", 0.783),
    Node("C", 1.577),
    Node("D", 0.150)
  )

  val nodes2 = List(
    Node("A"),
    Node("B"),
    Node("C")
  )

  val links2 = List(
    Edge(nodes2(0), nodes2(1)),
    Edge(nodes2(0), nodes2(2)),
    Edge(nodes2(1), nodes2(2)),
    Edge(nodes2(2), nodes2(0))
  )

  val graph2 = Graph(nodes2, links2)
  val prResult2 = List(
    Node("A", 1.076),
    Node("B", 0.769),
    Node("C", 1.153)
  )

  val pagerank1 = graph1.pagerank()
  val pagerank2 = graph2.pagerank(0.000001, 100, 0.5)

  val pagerank3 = graph1.parPagerank()
  val pagerank4 = graph2.parPagerank(0.000001, 100, 0.5)

  val pagerank5 = graph1.jungPagerank

  "Pagerank is correct for both graphs" in {
    pagerank1.map(x => x.rank).zip(prResult1.map(y => y.rank)).foreach(a => a._1 must beCloseTo(a._2, 0.01))
    pagerank2.map(x => x.rank).zip(prResult2.map(y => y.rank)).foreach(a => a._1 must beCloseTo(a._2, 0.01))

    pagerank3.map(x => x.rank).zip(prResult1.map(y => y.rank)).foreach(a => a._1 must beCloseTo(a._2, 0.01))
    pagerank4.map(x => x.rank).zip(prResult2.map(y => y.rank)).foreach(a => a._1 must beCloseTo(a._2, 0.01))
  }
}
