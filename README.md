## Shapefile Parsing for Scala

### Overview
Scala Shapefile is an experimental library that provides support for
parsing shapefiles.

### What's supported now?

All standard X,Y geometries. Only .shp files are supported. The index
file (.shx), projection file (.proj) and database file (.dbf) are
currently ignored.

### What will be supported (at some point)?

Indexed access and DBF will be added soon. Lazy access will also be
added before too long.

### Example

```scala
import geotrellis.shapefile.{Parser,Record}

val p:Seq[Record] = shapefile.Parser("src/test/resources/UScounties.shp")
```

Each record contains an ```id``` and jts ```Geometry```