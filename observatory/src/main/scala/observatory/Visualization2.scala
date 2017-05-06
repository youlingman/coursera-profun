package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Interaction.tileLocation
import observatory.Visualization.interpolateColor

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param x X coordinate between 0 and 1
    * @param y Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
                             x: Double,
                             y: Double,
                             d00: Double,
                             d01: Double,
                             d10: Double,
                             d11: Double
                           ): Double = {
    d00 * (1 - x) * (1 - y) + d10 * x * (1 - y) + d01 * (1 - x) * y + d11 * x * y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param zoom Zoom level of the tile to visualize
    * @param x X value of the tile to visualize
    * @param y Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
                     grid: (Int, Int) => Double,
                     colors: Iterable[(Double, Color)],
                     zoom: Int,
                     x: Int,
                     y: Int
                   ): Image = {
    // use bilinearInterpolation and giving grid to predict temperature
    def predictTemperature(location: Location): Double = {
      val lat0 = location.lat.floor.toInt
      val lat1 = location.lat.ceil.toInt
      val lon0 = location.lon.floor.toInt
      val lon1 = location.lon.ceil.toInt
      bilinearInterpolation(location.lat - lat0, location.lon - lon0, grid(lat0, lon0), grid(lat0, lon1), grid(lat1, lon0), grid(lat1, lon1))
    }

    val width = 256
    val height = 256

    val pixels = {
      for (j <- 0 until height; i <- 0 until width)
        yield interpolateColor(colors, predictTemperature(tileLocation(zoom + 8, 256 * x + i, 256 * y + j)))
    }.map(c => Pixel(c.red, c.green, c.blue, 127))

    Image(width, height, pixels.toArray)
  }

}
