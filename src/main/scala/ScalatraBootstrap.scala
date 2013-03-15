import org.ghtorrent.netviz._
import org.scalatra._
import javax.servlet.ServletContext

class ScalatraBootstrap extends LifeCycle with DataLoader {

  override def init(context: ServletContext) {

    context.mount(new GHTorrentNetViz(load), "/*")
  }

  def dataLocation: String = ???
}
