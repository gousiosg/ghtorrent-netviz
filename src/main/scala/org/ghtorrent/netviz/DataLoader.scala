package org.ghtorrent.netviz

import io.Source
import collection.parallel.immutable
import collection.mutable

case class Commit(id: Int, project: Project,
                  developer: Developer, timestamp: Int)
case class Project(id: Int, name: String, lang: Lang)
case class Lang(id: Int, name: String)
case class Developer(id: Int, name: String)

case class Data(commits: immutable.ParSeq[Commit],
                projects: immutable.ParSeq[Project],
                langs: immutable.ParSeq[Lang],
                devs: immutable.ParSeq[Developer])

trait DataLoader {

  def dataLocation: String

  def load() : Data =
    load(Source.fromFile(dataLocation))

  def load(s: Source) : Data = {

    val projects = mutable.Map[String, Project]()
    val langs    = mutable.Map[String, Lang]()
    val devs     = mutable.Map[Int, Developer]()
    val commits  = mutable.MutableList[Commit]()

    var langCounter = 0
    var commitCounter = 0

    for (l <- s.getLines().drop(1)) {
      val fields = l.split(",")

      if (fields(6).size != 8 || !fields(6).matches("^[A-Z]+$")) {

        val lang = langs.get(fields(4)) match {
          case Some(x) => x
          case None =>
            langCounter += 1
            langs.put(fields(4), Lang(langCounter, fields(4)))
            langs.get(fields(4)).get
        }

        val project = projects.get(fields(1)) match {
          case Some(x) => x
          case None =>
            projects.put(fields(1), Project(Integer.parseInt(fields(1)), fields(3), lang))
            projects.get(fields(1)).get
        }

        val devId = Integer.parseInt(fields(5))
        val commiter = devs.get(devId) match {
          case Some(x) => x
          case None =>
            devs.put(devId, Developer(devId, fields(6)))
            devs.get(devId).get
        }

        commitCounter += 1
        commits += Commit(commitCounter, project, commiter, Integer.parseInt(fields(7)))
      }
    }

    Data(commits.toList.par, projects.values.toList.par,
         langs.values.toList.par, devs.values.toList.par)
  }
}
