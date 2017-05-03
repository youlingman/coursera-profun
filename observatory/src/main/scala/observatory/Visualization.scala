package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    def greatCircleDistance(a: Location, b: Location): Double = Math.sqrt(Math.pow(a.lat - b.lat, 2) + Math.pow(a.lon - b.lon, 2))
    // use inverse distance weighting(IDW) to interpolation
    val p = 2

    val distancePair = temperatures.map(t => (Math.pow(greatCircleDistance(t._1, location), -p), t._2))
    val totalDistance = distancePair.map(_._1).sum
    distancePair.map(t => t._1 * t._2).sum / totalDistance
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    def linearInterpolate(x1: Double, y1: Int, x2: Double, y2: Int, x: Double): Int = ((x - x1) * (y2 - y1) / (x2 - x1)).toInt + y1

    def _interpolateColor(pair: ((Double, Color), (Double, Color)), value: Double): Color = {
      val point_1 = pair._1
      val point_2 = pair._2
      Color(linearInterpolate(point_1._1, point_1._2.red, point_2._1, point_2._2.red, value),
        linearInterpolate(point_1._1, point_1._2.green, point_2._1, point_2._2.green, value),
        linearInterpolate(point_1._1, point_1._2.blue, point_2._1, point_2._2.blue, value))
    }

    // pick out point pair to made linear interpolate
    // may change to other regress algorithm get better performance?
    val sortedPoints = points.toList.sortBy(_._1)
    _interpolateColor(sortedPoints.zip(sortedPoints.tail).
      find(t => value >= t._1._1 && value <= t._2._1).
      getOrElse(if (value < sortedPoints.head._1) (sortedPoints.head, sortedPoints.tail.head) else (sortedPoints.drop(1).last, sortedPoints.last)), value)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    def genLocation(x: Int, y: Int): Location = Location(x, y)

    val width = 360
    val height = 180
    val pixels = new Array[Pixel](width * height)

    for (y <- 0 to height; x <- 0 to width) {
      val color = interpolateColor(colors, predictTemperature(temperatures, genLocation(x, y)))
      pixels(x + y * height) = Pixel(color.red, color.green, color.blue, 255)
    }
    Image(width, height, pixels)
  }

}

