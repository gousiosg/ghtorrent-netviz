package org.ghtorrent.netviz

import org.scalatra.{BadRequest, NotFound}

class GHTorrentNetViz extends GHTorrentNetVizStack with DataLoader {

  val data = load

  get("/langs") {
    data.langs
  }

  get("/langsearch") {
    data.langs.filter(l => l.name.startsWith(params("q")))
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
        data.commits.filter {
          c => c.project.lang.id == langId
               c.timestamp > from &&
               c.timestamp < to
        }.groupBy {
          x => x.developer.id
        }.values.map {
          x => x.map{y => y.project.id}.distinct
        }.map {
          x => x.toList.combinations(2).toList
        }.flatten.take(5000).map {
          x => Edge(x.head, x.tail.head)
        }.toList.distinct
      case Some(x) => NotFound("Language " + x + " not found")
      case None => BadRequest("Missing required parameter l")
    }
  }

  def dataLocation: String = System.getProperty("data.file")
}

case class Edge(x: Int, y: Int)