package org.ghtorrent.netviz

import org.specs2.mutable._
import io.Source

class DataLoaderSpec extends Specification with DataLoader {

  def dataLocation: String = ???

  "The parsed file" should {

    val data = load(Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("data.txt")))

    "contain 72 projects" in {
      data.projects must have length (72)
    }

    "contain 1 languages" in {
      data.langs must have length (1)
    }

    "contain 77 developers" in {
      data.devs must have length (77)
    }

    "contain 9999 commits" in {
      data.commits must have size (9999)
    }
  }
}
