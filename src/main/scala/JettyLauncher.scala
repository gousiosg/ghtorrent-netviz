import org.ghtorrent.netviz._

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.DefaultServlet
import org.eclipse.jetty.webapp.WebAppContext

object JettyLauncher {
def main(args: Array[String]) {
  val port = if(System.getenv("PORT") != null) System.getenv("PORT").toInt else 8080

  val server = new Server(port)
  val context = new WebAppContext()
  context setContextPath "/"
  context.setResourceBase("src/main/webapp")
  context.addServlet(classOf[GHTorrentNetVizStack], "/*")
  context.addServlet(classOf[DefaultServlet], "/")

  server.setHandler(context)

  server.start
  server.join
}
}