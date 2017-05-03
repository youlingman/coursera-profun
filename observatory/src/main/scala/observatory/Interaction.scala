package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x    X coordinate
    * @param y    Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val n = Math.pow(2, zoom)
    val latRad = Math.atan(Math.sinh(Math.PI * (1 - 2 * y / n)))
    Location(x / n * 360 - 180, latRad * 180 / Math.PI)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param zoom         Zoom level
    * @param x            X coordinate
    * @param y            Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val width = 256
    val height = 256
    val pixels = new Array[Pixel](width * height)

    for (i <- 0 to width; j <- 0 to height) {
      val color = Visualization.interpolateColor(colors, Visualization.predictTemperature(temperatures, tileLocation(zoom + 8, 256 * x + i, 256 * y + j)))
      pixels(i + j * height) = Pixel(color.red, color.green, color.blue, 127)
    }
    Image(width, height, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Int, Data)],
                           generateImage: (Int, Int, Int, Int, Data) => Unit
                         ): Unit = {
    for (yearData <- yearlyData; zooms <- 0 to 3; x <- 0 until Math.pow(2, zooms).toInt; y <- 0 until Math.pow(2, zooms).toInt) {
      generateImage(yearData._1, zooms, x, y, yearData._2)
    }
  }

}
