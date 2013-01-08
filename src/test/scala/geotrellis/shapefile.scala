package geotrellis

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import com.vividsolutions.jts.geom._

class ShapefileSpec extends FlatSpec with ShouldMatchers {

  implicit val g: GeometryFactory = new GeometryFactory()

  "Shapefile parser" should "parse a shapefile" in {
    val p = shapefile.Parser("src/test/resources/UScounties.shp")
    println(p.map(_.id))
  }

}
