package org.ghtorrent.netviz

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