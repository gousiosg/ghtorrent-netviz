package org.ghtorrent.netviz

import io.Source
import collection.parallel.immutable
import collection.mutable
import org.slf4j.LoggerFactory

case class Commit(id: Int, project: Project,
                  developer: Developer, timestamp: Int)
case class Project(id: Int, name: String, lang: Lang)
case class Lang(id: Int, name: String)
case class Developer(id: Int, name: String)

case class Data(commits: immutable.ParSeq[Commit],
                projects: immutable.ParSeq[Project],
                langs: immutable.ParSeq[Lang],
                devs: immutable.ParSeq[Developer])

/*

select p.id as project_id, u.login as project_owner, p.name as project_name, u1.id as commiter_id, u1.login as committer, p.language as lang, unix_timestamp(c.created_at) as timestamp from projects p, users u, commits c, project_commits pc, users u1 where pc.commit_id = c.id and pc.project_id = p.id and p.forked_from is null and p.deleted is false and p.language is not null and c.author_id = u1.id and p.owner_id = u.id and u1.login not regexp('^[A-Z]{8}$') order by c.created_at
 */

trait DataLoader {

  private val log = LoggerFactory.getLogger("Loader")

  def dataLocation: String

  def load() : Data =
    load(Source.fromFile(dataLocation))

  def load(s: Source) : Data = {
    val ts = System.currentTimeMillis
    val projects = mutable.Map[String, Project]()
    val langs    = mutable.Map[String, Lang]()
    val devs     = mutable.Map[Int, Developer]()
    val commits  = mutable.MutableList[Commit]()

    var langCounter = 0
    var commitCounter = 0

    for (l <- s.getLines().drop(1)) {
      //Line format:
      //        0           1             2           3          4        5        6
      //  project_id,project_owner,project_name,committer_id,committer,language,timestamp
      val fields = l.split(",")

      if (fields(4).size != 8 || !fields(4).matches("^[A-Z]{8}$")) {

        val lang = langs.get(fields(5)) match {
          case Some(x) => x
          case None =>
            langCounter += 1
            langs.put(fields(5), Lang(langCounter, fields(5)))
            langs.get(fields(5)).get
        }

        val project = projects.get(fields(0)) match {
          case Some(x) => x
          case None =>
            projects.put(fields(0), Project(Integer.parseInt(fields(0)), fields(1) + "/" +fields(2), lang))
            projects.get(fields(0)).get
        }

        val devId = Integer.parseInt(fields(3))
        val commiter = devs.get(devId) match {
          case Some(x) => x
          case None =>
            devs.put(devId, Developer(devId, fields(4)))
            devs.get(devId).get
        }

        commitCounter += 1
        commits += Commit(commitCounter, project, commiter, Integer.parseInt(fields(6)))
      }
    }

    val totalTime = System.currentTimeMillis - ts
    val numCommits = commits.size
    val numProjects = projects.size
    val numLangs = langs.size
    val numDevs = devs.size
    log.info(s"\n\t$numCommits commits\n\t$numProjects projects\n\t$numLangs languages\n\t$numDevs developers\n\tin $totalTime ms")
    Data(commits.toList.par, projects.values.toList.par,
         langs.values.toList.par, devs.values.toList.par)
  }
}
