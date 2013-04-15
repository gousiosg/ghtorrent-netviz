package org.ghtorrent.netviz

import org.scalatra.test.specs2._

class GHTorrentNetVizSpec extends MutableScalatraSpec {

  addServlet(classOf[GHTorrentNetViz], "/*")
  System.setProperty("data.file", "/Users/gousiosg/Developer/github-network-viz/src/test/resources/data.txt")

  "GET /links on GHTorrentNetVizServlet" should {
    "return status 200 if called without params" in {
      get("/links") {
        status must_== 200
      }
    }
    "return status 200 if called with unknown language" in {
      get("/links?l=foobar") {
        status must_== 200
      }
    }
    "return status 200 if called correctly" in {
      get("/links?l=Ruby") {
        status must_== 200
      }
    }
    "return status 200 if called correctly" in {
      get("/links?l=Ruby") {
        status must_== 200
      }
    }
  }

  "GET /project on GHTorrentNetVizServlet" should {
    "return status 400 if called without params" in {
      get("/project") {
        status must_== 400
      }
    }
    "return status 404 if called with unknown project" in {
      get("/project?p=12345") {
        status must_== 404
      }
    }
    "return status 200 if called correctly" in {
      get("/project?p=2") {
        status must_== 200
      }
    }
  }
}
