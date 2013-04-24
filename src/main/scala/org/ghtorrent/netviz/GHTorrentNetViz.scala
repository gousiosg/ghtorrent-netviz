package org.ghtorrent.netviz

import org.json4s.{DefaultFormats, Formats}
import org.scalatra.json._

import org.scalatra.{BadRequest, NotFound}
import org.slf4j.LoggerFactory

class GHTorrentNetViz extends GHTorrentNetVizStack with DataLoader with JacksonJsonSupport {

  val reqTS = new ThreadLocal[Long]()
  protected implicit val log = LoggerFactory.getLogger("API")

  protected implicit val jsonFormats: Formats = DefaultFormats

  before() {
    contentType = formats("json")
    reqTS.set(System.currentTimeMillis)
  }

  after() {
    val time = System.currentTimeMillis() - reqTS.get
    log.info("time: " + time + " ms")
//    response.addHeader("Cache-Control", "must-revalidate, max-age=864000")
//    response.addHeader("Vary", "Accept-Encoding")
  }

  val data = load

  lazy val commitsPerProject =
    data.commits.groupBy(c => c.project.id).foldLeft(Map[Int, Int]())((acc, a) => acc ++ Map(a._1 -> a._2.length))

  lazy val projectLangs =
    data.projects.foldLeft(Map[Int, Lang]())((acc, a) => acc ++ Map(a.id -> a.lang))

  lazy val projectNames =
    data.projects.foldLeft(Map[Int, String]())((acc, a) => acc ++ Map(a.id -> a.name))

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

  get("/links") {
    val langs = Option(multiParams("l"))
    val prMethod = params.get("m")
    val numNodes = try {
      val nodes = Integer.parseInt(params("n"))
      if (nodes > 1000) 1000 else nodes
    } catch {case e: Exception => 100}
    val numEdges = try {
      Integer.parseInt(params("e"))
    } catch {case e: Exception => 5000}
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
        timer.tick("Filter timestamps and language: " + a.size + " commits")

        val b = a.groupBy {
          x => x.developer.id
        }.values
        timer.tick("Group commits by developer: " + b.size + " groups")

        val c = b.map {
          x => x.map{y => y.project}.distinct //<- don't change this to .toSet!
        }
        timer.tick("Convert commits -> projects: " + c.size + " project lists")

        // Generate combinations of projects based on the list of projects
        // that the developer has worked in. So if a developer has worked
        // in projects (a,b,c), we expect to see the following edges in the
        // generated graph: (a->b), (a->c), (b->c)
        val edges = c.map {
          // The combinations function returns an iterator so converting it to
          // List is expensive. Make the most common cases fast.
          x => x match {
            case y if y.size == 1 => List[List[Project]]()
            case y if y.size == 2 => List(y.toList)
            case _ => x.toList.combinations(2).toList
          }
        }.flatten.map{x => Edge(Node(x.head.id), Node(x.tail.head.id))}.toSet
        timer.tick("Building graph: edges: " + edges.size)

        val nodes = edges.flatMap{e => List(e.source, e.target)}.toSet
        timer.tick("Building graph: nodes: " + nodes.size)

        val graph = Graph(nodes.toList, edges.toList)

        val rank = prMethod match {
          case Some(m) if m == "par"  => log.info("PR algo: par");  graph.parPagerank(deltaPR = 0.001)
          case Some(m) if m == "jung" => log.info("PR algo: jung"); graph.jungPagerank
          case Some(m) if m == "def"  => log.info("PR algo: def");  graph.pagerank(deltaPR = 0.001)
          case Some(m) if m == "rank" => log.info("PR algo: rank"); graph.nodeRank
          case Some(m) => log.info("PR algo: " + m + " specified, unknown (rank)"); graph.jungPagerank
          case None => log.info("PR algo: unspecified (rank)"); graph.jungPagerank
        }
        timer.tick("Ranking nodes")

        // Index edges by source node
        val nodeIdx = edges.foldLeft(Map[Int, List[Edge[Int]]]().withDefaultValue(List[Edge[Int]]())) {
          (acc, e) =>
            acc ++ Map(e.source.name -> (e :: acc(e.source.name)))
        }

        // Construct a sorted array of ranked nodes
        val rankedNodes = rank.toArray.sortWith((a, b) => if (a.rank > b.rank) true else false).take(numNodes).distinct

        // Construct a list of edges that originate from nodes in the ranked nodes list
        val rankedEdges = rankedNodes.flatMap{x => nodeIdx(x.name)}

        // Filter out edges which contain nodes linked by just one edge.
        // This is to eliminate visual noise at the client side
        var ranks = rankedEdges.foldLeft(Map[Node[Int], Int]().withDefaultValue(0)){
          (acc, x) =>
            acc ++ Map(x.source -> (acc(x.source) + 1)) ++ Map(x.target -> (acc(x.target) + 1))
        }
        var filteredEdges = rankedEdges.filter{e => ranks(e.target) > 1 && ranks(e.source) > 1 }.distinct

        timer.tick("Filtering spikes: " + ranks.keys.size + " nodes, " + filteredEdges.size + " links")

        // Randomly remove excessive links up to the target of 5000 connections
        ranks = rankedEdges.foldLeft(Map[Node[Int], Int]().withDefaultValue(0)){
          (acc, x) =>
            acc ++ Map(x.source -> (acc(x.source) + 1)) ++ Map(x.target -> (acc(x.target) + 1))
        }

        filteredEdges = if (filteredEdges.size > numEdges) {
          val rng = new scala.util.Random()
          val result = new scala.collection.mutable.ArrayBuffer[Edge[Int]]()
          var added = 0

          while (added < numEdges) {
            val edge = filteredEdges(rng.nextInt(filteredEdges.size))
            if (ranks(edge.source) > 2 && ranks(edge.target) > 2) {
                result += edge
                added += 1
            }
          }
          timer.tick("Trimming down edges: " + ranks.keys.size + " nodes, " + result.length + " links")
          result.toArray
        } else {
          filteredEdges
        }

        // Convert nodes and edges to the d3.js format
        val allVertices = filteredEdges.flatMap {
          e => Array(e.source, e.target)
        }.distinct.map {
          f => Vertex(projectName(f.name), projectLang(f.name), numCommits(f.name),
            rankedNodes.find(p => p.name == f.name).getOrElse(Node[Int](0,0)).rank)
        }.sortWith((a, b) => if (a.rank > b.rank) true else false)

        val vertIdx = allVertices.foldLeft(Map("" -> -1)){(acc, x) => acc ++ Map(x.name -> (acc.values.max + 1))}.drop(0)
        val links = filteredEdges.map{x => Link(vertIdx(projectName(x.source.name)), vertIdx(projectName(x.target.name)))}.toList
        timer.tick("Preparing response: " + allVertices.length + " nodes, " + links.size + " links")

        D3jsGraph(allVertices, links)

      case None => BadRequest("Missing required parameter l")
    }
  }

  get("/hist") {
    val langs = Option(multiParams("l"))
    langs match {
      case Some(x) =>
        val langIds = x.foldLeft(Set[Int]()){
          (acc, x) =>
            data.langs.find(y => x.equalsIgnoreCase(y.name)) match {
              case Some(z) => acc + z.id
              case None => acc
            }
        }

        val commits = data.commits.filter {
          c => langIds.contains(c.project.lang.id)
        }

        var min = commits.minBy(c => c.timestamp).timestamp
        var max = commits.maxBy(c => c.timestamp).timestamp

        if (min < 946684800) // 1.1.2000
          min = 946684800

        if (max > System.currentTimeMillis() / 1000)
          max = (System.currentTimeMillis() / 1000).toInt

        val numWeeks = (max - min) / 604800
        println("numWeeks: " + numWeeks)
        langIds.map {
          l =>
            val existingbins = commits.filter(c => c.project.lang.id == l).foldLeft(Vector[Int]()) {
              (acc, x) =>
                if (min < x.timestamp && x.timestamp < max)
                  acc :+ (x.timestamp - min) / 604800
                else
                  acc
            }.groupBy {
              x => x
            }

            // Calculate the set difference between the expected and actual
            // bins and create a new Map with empty values to be merged with
            // the calculated bin map
            val missingbins = ((0 to numWeeks).toSet &~ existingbins.keySet).foldLeft(Map[Int, Vector[Int]]()){
              (acc, x) => acc ++ Map(x -> Vector[Int]())
            }

            (missingbins ++ existingbins).map {
              x =>
                TimeBin((min + (x._1 * 604800) + 604800) * 1000L, x._2.size,
                  data.langs.find(lang => lang.id == l).getOrElse(Lang(12345, "UNKNOWN")).name)
            }
        }.flatten.toList.sortWith { (a,b) => a.date < b.date }

      case None => BadRequest("Missing required parameter l")
    }
  }

  get("/project") {
    params.get("p") match {
      case Some(id) =>
        data.projects.find(p => p.id == Integer.parseInt(id)) match {
          case Some(x) => x
          case None => NotFound
        }
      case None => BadRequest("Missing required parameter p")
    }
  }

  def dataLocation: String = System.getProperty("data.file")

  def numCommits(x: Int) = commitsPerProject(x)
  def projectLang(x: Int) = projectLangs(x).name
  def projectName(x: Int) = projectNames(x)
}

case class TimeBin(date: Long, count: Int, lang: String)

// d3.js representation of graphs
// https://github.com/mbostock/d3/wiki/Force-Layout#wiki-nodes
case class D3jsGraph(nodes: Array[Vertex], links: List[Link])
case class Vertex(name: String, lang: String, commits: Int, rank: Double)
case class Link(source: Int, target: Int)
