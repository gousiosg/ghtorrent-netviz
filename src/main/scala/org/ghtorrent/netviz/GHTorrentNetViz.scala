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
    val lang = Option(params("l"))
    val from = try{Integer.parseInt(params("f"))} catch {case e: Exception => 0}
    val to   = try{Integer.parseInt(params("t"))} catch {case e: Exception => Integer.MAX_VALUE}

    lang match {
      case Some(x) if(data.langs.find(y => x.equalsIgnoreCase(y.name)).isDefined) =>
        val langId = data.langs.find(y => x.equalsIgnoreCase(y.name)).get.id
        val timer = Timer(System.currentTimeMillis)

        val a = data.commits.filter {
          c => c.project.lang.id == langId &&
               c.timestamp > from &&
               c.timestamp < to
        }
        timer.tick("Filter timestamps and language: " + a.size + " commits")(log)

        val b = a.groupBy {
          x => x.developer.id
        }.values
        timer.tick("Group commits by developer: " + b.size + " groups")(log)

        val c = b.map {
          x => x.map{y => y.project.id}.distinct
        }
        timer.tick("Convert commits -> projects: " + c.size + " project lists")(log)

        val d = c.map {
          /* The combinations function returns an iterator so converting it to
           * List is expensive. Make the most common cases fast.
           */
          x => x match {
            case y if y.size == 1 => List[List[Int]]()
            case y if y.size == 2 => List(y.toList)
            case _ => x.toList.combinations(2).toList
          }
        }.flatten.toParArray.distinct
        timer.tick("Distinct pairs of projects: " + d.size + " pairs")(log)

        val e = d.take(5000).map {
          x => Edge(x.head, x.tail.head)
        }.toList
        timer.tick("Generate edges: " + e.size + " edges")(log)

        e
      case Some(x) => NotFound("Language " + x + " not found")
      case None => BadRequest("Missing required parameter l")
    }
  }

  def log[T](col: ParSeq[T]): ParSeq[T] = {

    col
  }

  get("/hist") {
    timebins
  }

  def dataLocation: String = System.getProperty("data.file")
}

case class TimeBin(start: Int, end: Int, count: Int)
case class Edge(x: Int, y: Int)

case class Timer(start: Long) {
  var tick = System.currentTimeMillis

  def tick(stage: String)(implicit log: org.slf4j.Logger) {
    val now = System.currentTimeMillis
    val diffFromStart = now - start
    val diff = now - tick
    tick = now

    log.info(stage + " " + diff + "ms, total: " + diffFromStart + "ms")
  }
}