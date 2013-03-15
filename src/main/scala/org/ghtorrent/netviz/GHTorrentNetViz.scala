package org.ghtorrent.netviz

class GHTorrentNetViz(data: Data) extends GHTorrentNetVizStack {

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
}
