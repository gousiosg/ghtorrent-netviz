package org.ghtorrent.netviz

import org.json4s.{DefaultFormats, Formats}
import org.scalatra.json._

import org.scalatra.{BadRequest, NotFound}
import org.slf4j.LoggerFactory
import scala.collection.parallel.immutable.ParSeq

class GHTorrentNetViz extends GHTorrentNetVizStack with DataLoader with JacksonJsonSupport {

  val reqTS = new ThreadLocal[Long]()
  val log = LoggerFactory.getLogger("API")

  protected implicit val jsonFormats: Formats = DefaultFormats

  before() {
    contentType = formats("json")
    reqTS.set(System.currentTimeMillis)
  }

  after() {
    val time = System.currentTimeMillis() - reqTS.get
    log.info("time: " + time + " ms")
  }

  val data = load

  lazy val timebins = {
    var min = data.commits.minBy(c => c.timestamp).timestamp
    var max = data.commits.maxBy(c => c.timestamp).timestamp

    if (min < 946684800) // 1.1.2000
      min = 946684800

    if (max > System.currentTimeMillis() / 1000)
      max = (System.currentTimeMillis() / 1000).toInt

    val bins = data.commits.foldLeft(Vector[Int]()) {
      (acc, x) =>
        if (min < x.timestamp && x.timestamp < max)
          acc :+ (x.timestamp - min) / 604800
        else
          acc
    }.groupBy {
      x => x
    }.map {
      x =>
        TimeBin(min + (x._1 * 604800), min + (x._1 * 604800) + 604800, x._2.size)
    }.toList.sortWith{
      (a,b) =>
        a.start < b.start
    }
    log.info(bins.size + " histogram bins")
    bins
  }

  get("/langs") {
    data.langs.toList
  }

  get("/langsearch") {
    params("q") match {
      case q : String =>
        data.langs.filter(l => l.name.toLowerCase.startsWith(q.toLowerCase)).toList
      case _ =>
        log.info("Cannot find parameter q")
        List()
    }
  }

  get("/projects") {
    val lang = Integer.parseInt(params("l"))
    val from = Integer.parseInt(params("f"))
    val to   = Integer.parseInt(params("t"))

    data.commits.filter {
      c =>
        c.project.lang.id == lang &&
        c.timestamp > to &&
        c.timestamp < from
    }.foldLeft(Set[Project]()) {
      (acc, x) =>
        acc + x.project
    }.toList
  }

  get("/links") {
    val langs = Option(multiParams("l"))
    val from = try{Integer.parseInt(params("f"))} catch {case e: Exception => 0}
    val to   = try{Integer.parseInt(params("t"))} catch {case e: Exception => Integer.MAX_VALUE}

    langs match {
      case Some(x) =>
        val langIds = x.foldLeft(Set[Int]()){
          (acc, x) =>
            data.langs.find(y => x.equalsIgnoreCase(y.name)) match {
              case Some(z) => acc + z.id
              case None => acc
            }
        }

        val timer = Timer(System.currentTimeMillis)

        val a = data.commits.filter {
          c => langIds.contains(c.project.lang.id) &&
               c.timestamp > from &&
               c.timestamp < to
        }
        timer.tick("Filter timestamps and language: " + a.size + " commits")(log)

        val b = a.groupBy {
          x => x.developer.id
        }.values
        timer.tick("Group commits by developer: " + b.size + " groups")(log)

        val c = b.map {
          x => x.map{y => y.project}.distinct
        }
        timer.tick("Convert commits -> projects: " + c.size + " project lists")(log)

        val d = c.map {
          /* The combinations function returns an iterator so converting it to
           * List is expensive. Make the most common cases fast.
           */
          x => x match {
            case y if y.size == 1 => List[List[Project]]()
            case y if y.size == 2 => List(y.toList)
            case _ => x.toList.combinations(2).toList
          }
        }.flatten.toSet
        timer.tick("Distinct pairs of projects: " + d.size + " pairs")(log)

        val nodes = d.foldLeft(Set[Project]()){
          (acc, x) => acc + x.head + x.tail.head
        }.map {
          x => Node[NodeInfo](NodeInfo(x.id, x.lang))
        }.toArray

        timer.tick("Graph nodes (projects): " + nodes.size + " nodes")(log)

        val nodeIdxs = (0 to (nodes.size - 1)).foldLeft(Map[Node[NodeInfo], Int]()){(acc, i) => acc ++ Map(nodes(i) -> i)}

        val edges = d.map {
          x =>
            Link(nodes(nodeIdxs(Node(NodeInfo(x.head.id, x.head.lang)))),
              nodes(nodeIdxs(Node(NodeInfo(x.tail.head.id, x.tail.head.lang)))))
        }.toList
        timer.tick("Generate graph: " + edges.size + " edges")(log)

        val graph = Graph(nodes, edges)
        val rank = graph.pagerank(deltaPR = 0.01).sortWith((a,b) => if(a.rank > b.rank) true else false)
        timer.tick("Running pagerank")(log)

        rank.take(50)
      case None => BadRequest("Missing required parameter l")
    }
  }

  get("/hist") {
    timebins
  }

  def dataLocation: String = System.getProperty("data.file")
}

case class TimeBin(start: Int, end: Int, count: Int)

// d3.js representation of graphs
// https://github.com/mbostock/d3/wiki/Force-Layout#wiki-nodes
case class NodeInfo(pid: Int, lang: Lang)
case class Edge(source: Int, target: Int)
