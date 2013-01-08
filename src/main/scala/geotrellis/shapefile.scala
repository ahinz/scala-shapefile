package geotrellis

import scala.annotation.tailrec

import com.vividsolutions.jts.geom._

import java.nio.channels.FileChannel
import java.nio.{ByteBuffer,ByteOrder}
import java.io.RandomAccessFile

package object shapefile {

  trait TypeMapper {
    val types = Seq(NullParser,PointParser,PolyLineParser,PolygonParser,MultiPointParser)
    def getType(n: Int):Either[Int,ShapeParser] = types.filter(_.shapeType == n).headOption match {
      case Some(t) => Right(t)
      case None => Left(n)
    }
  }

  trait ShapefileUtil {
    final def little(b: ByteBuffer) = b.order(ByteOrder.LITTLE_ENDIAN)
    final def big(b: ByteBuffer) = b.order(ByteOrder.BIG_ENDIAN)
    final def skip(b: ByteBuffer, nBytes: Int) = b.position(b.position + nBytes)
    final def skipBbox(b: ByteBuffer) = skip(b, 32) // skip 4 doubles (32 bytes)
  }

  case class Record(id: Int, g: Geometry)

  trait ShapeParser extends ShapefileUtil {
    val shapeType: Int

    def apply(b: ByteBuffer)(implicit g: GeometryFactory): Geometry
    def parse(b: ByteBuffer)(implicit g: GeometryFactory): Geometry = apply(b)

    def parseRecord(b: ByteBuffer)(implicit g: GeometryFactory): Record = {
      val rid = big(b).getInt
      val clen = big(b).getInt
      val shpTyp = little(b).getInt

      shpTyp match {
        case NullParser.shapeType => Record(rid, NullParser(b))
        case t if t == shapeType => Record(rid, parse(b))
        case t => sys.error(s"Expected shape type ${shapeType}, got $t")
      }
    }
  }

  object NullParser extends ShapeParser {
    lazy val shapeType = 0

    def apply(b: ByteBuffer)(implicit g: GeometryFactory) = {
      g.createPoint(null:Coordinate)
    }
  }

  object PointParser extends ShapeParser {
    lazy val shapeType = 1

    def apply(b: ByteBuffer)(implicit g: GeometryFactory) = {
      g.createPoint(new Coordinate(little(b).getDouble, b.getDouble))
    }
  }

  object MultiPointParser extends ShapeParser {
    lazy val shapeType = 8

    def apply(b: ByteBuffer)(implicit g: GeometryFactory) = {
      skipBbox(b)
      g.createMultiPoint(
        Array.ofDim[Point](little(b).getInt) map { _ => PointParser(b) })
    }
  }

  trait PolyThingParser extends ShapeParser {
    def parseCoordinate(b: ByteBuffer) = new Coordinate(little(b).getDouble, b.getDouble)

    // rewrite as fold?
    @tailrec
    final def take[T](v: Seq[T], s: Seq[Int], a: Seq[Seq[T]]):Seq[Seq[T]] = s match {
      case Nil => a
      case Seq(x, xs@_*) => v.splitAt(x) match {
        case (n,r) => take(r, xs, r +: a)
      }
    }

    type A

    def pointsToInnerGeom(a: Seq[Coordinate])(implicit g: GeometryFactory):A
    def buildCollection(a: Seq[A])(implicit g: GeometryFactory):Geometry

    def apply(b: ByteBuffer)(implicit g: GeometryFactory) = {
      skipBbox(b)
      val nParts = little(b).getInt
      val nPts = little(b).getInt
      val terminals = (0 until nParts).map { _ => b.getInt }
      val tOffsets = terminals.zip(0 +: terminals).map { case (a,b) => a - b }

      val points = (0 until nPts) map { _ => parseCoordinate(b) }
      val strings = take(points, tOffsets, Seq.empty) map { c => pointsToInnerGeom(c) }

      buildCollection(strings)
    }
  }

  object PolyLineParser extends PolyThingParser {
    lazy val shapeType = 3

    type A = LineString
    def pointsToInnerGeom(a: Seq[Coordinate])(implicit g: GeometryFactory) = g.createLineString(a.toArray)
    def buildCollection(a: Seq[LineString])(implicit g: GeometryFactory) = g.createMultiLineString(a.toArray)
  }

  object PolygonParser extends PolyThingParser {
    lazy val shapeType = 5

    type A = LinearRing
    def pointsToInnerGeom(a: Seq[Coordinate])(implicit g: GeometryFactory) = 
      g.createLinearRing((a :+ a.head).toArray)

    def buildCollection(a: Seq[LinearRing])(implicit g: GeometryFactory) = 
      g.createPolygon(a.head, a.tail.toArray)
  }

  class ShapefileParser extends ShapefileStructure with TypeMapper

  object Parser {
    def apply(f: String)(implicit g: GeometryFactory) = new ShapefileParser().parse(f)(g)
  }

  trait ShapefileStructure extends ShapefileUtil { this: TypeMapper =>

    case class BBox(xMin: Double, yMin: Double, xMax: Double, yMax: Double,
      zMin: Double, zMax: Double, mMin: Double, mMax: Double)

    // xmin,ymin,xmax,ymax,zmin,zmax,min,mmax
    def parseBbox(b: ByteBuffer) =
      BBox(b.getDouble, b.getDouble, b.getDouble, b.getDouble,
        b.getDouble, b.getDouble, b.getDouble, b.getDouble)

    def parseMagicNumber(b: ByteBuffer) {
      if (big(b).getInt != 9994)
        sys.error("Expected 9994 (shapefile magic number)")
    }

    def parseVersion(b: ByteBuffer) {
      if (little(b).getInt != 1000)
        sys.error("Expected 1000 (invalid version)")
    }

    def parseShapeType(b: ByteBuffer):ShapeParser = getType(little(b).getInt) match {
      case Right(t) => t
      case Left(t) => sys.error(s"Couldn't parse shape type ($t)")
    }

    def parse(fileName: String)(implicit g: GeometryFactory):Seq[Record] = {
      val inChannel = new RandomAccessFile(fileName, "r").getChannel()
      val buffer = big(inChannel.map(FileChannel.MapMode.READ_ONLY, 0, inChannel.size()))

      parseMagicNumber(buffer)
      skip(buffer, 20) // skip 5 ints
      buffer.getInt // ignore length
      parseVersion(buffer)

      val shapeParser = parseShapeType(buffer)
      val bbox = parseBbox(buffer)

      var gs = Seq.empty[Record]
      while(buffer.position < buffer.limit)
        gs = gs :+ shapeParser.parseRecord(buffer)

      inChannel.close()
      gs
    }
  }
}
