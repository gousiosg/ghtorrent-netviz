package org.ghtorrent.netviz

import org.json4s.{DefaultFormats, Formats}
import org.scalatra.json._

import org.scalatra.{BadRequest, NotFound}
import org.slf4j.LoggerFactory

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
    val prMethod = params.get("m")
    val numNodes = try {
      val nodes = Integer.parseInt(params("n"))
      if (nodes > 1000) 1000 else nodes
    } catch {case e: Exception => 1000}
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
          x => Node[Int](x.id)
        }

        timer.tick("Graph nodes (projects): " + nodes.size + " nodes")(log)

        val prIdx = nodes.foldLeft(Map[Int, Node[Int]]()){(acc, x) => acc ++ Map(x.name -> x)}
        val edges = d.map {x => Edge(prIdx(x.head.id), prIdx(x.tail.head.id))}

        val graph = Graph(nodes.toList, edges.toList)

        timer.tick("Building graph: " + edges.size + " edges")(log)

        val rank = prMethod match {
          case Some(m) if m == "par" => log.info("PR algo: par"); graph.parPagerank(deltaPR = 0.01)
          case Some(m) if m == "jung" => log.info("PR algo: jung"); graph.jungPagerank
          case Some(m) if m == "def" => log.info("PR algo: def"); graph.pagerank(deltaPR = 0.01)
          case Some(m) => log.info("PR algo: " + m + " specified, unknown (jung)"); graph.jungPagerank
          case None => log.info("PR algo: unspecified (jung)"); graph.jungPagerank
        }

        timer.tick("Running pagerank")(log)

        val rankedNodes = rank.toList.sortWith((a, b) => if (a.rank > b.rank) true else false).take(numNodes)
        val rankedEdges = rankedNodes.map(e => e.name).flatMap(x => edges.filter(y => y.source.name == x))

        val vertices = rankedEdges.flatMap {
          x => List(x.source, x.target)
        }.distinct.map {
          x => Vertex(x.name, projectLang(x.name), numCommits(x.name))
        }.toArray.sortWith((a, b) => if (a.pid > b.pid) true else false)

        timer.tick("Retrieving project info per node")(log)

        val vertIdx = vertices.foldLeft(Map(0 -> -1)){(acc, x) => acc ++ Map(x.pid -> (acc.values.max + 1))}.drop(0)
        val links = rankedEdges.map{x => Link(vertIdx(x.source.name), vertIdx(x.target.name))}.toList

        timer.tick("Preparing response")(log)
        D3jsGraph(vertices, links)

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
case class D3jsGraph(nodes: Array[Vertex], links: List[Link])
case class Vertex(pid: Int, lang: String, commits: Int)
case class Link(source: Int, target: Int)
