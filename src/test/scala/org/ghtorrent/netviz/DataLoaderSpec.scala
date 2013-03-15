package org.ghtorrent.netviz

import org.specs2.mutable._
import io.Source

class DataLoaderSpec extends Specification with DataLoader {

  def dataLocation: String = ???

  "The parsed file" should {

    val data = load(Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("data.txt")))

    "contain 5 projects" in {
      data.projects must have length (5)
    }

    "contain 2 languages" in {
      data.langs must have length (2)
    }

    "contain 137 developers" in {
      data.devs must have length (137)
    }

    "contain 7961 commits" in {
      data.commits must have size (7961)
    }
  }

}
