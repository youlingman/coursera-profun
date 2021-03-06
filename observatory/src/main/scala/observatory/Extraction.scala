package observatory

import java.nio.file.Paths
import java.time.LocalDate

import org.apache.spark.rdd.RDD
import org.apache.spark.sql.catalyst.ScalaReflection
import org.apache.spark.sql.types.StructType
import org.apache.spark.sql.{Row, SparkSession}
import org.apache.spark.{SparkConf, SparkContext}

/**
  * 1st milestone: data extraction
  */
object Extraction {

  //  @transient lazy val conf: SparkConf = new SparkConf().setMaster("local").setAppName("observatory")
  //  @transient lazy val sc: SparkContext = new SparkContext(conf)

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("observatory")
      .master("local")
      .getOrCreate()

  // For implicit conversions like converting RDDs to DataFrames
  import spark.implicits._

  case class Station(STN: String, WBAN: String, latitude: Double, longitude: Double)

  case class Temperature(STN: String, WBAN: String, month: Int, day: Int, temperature: Double)

  //  val StationType = new StructType().add("STN", "string").add("WBAN", "string").add("latitude", "double").add("longitude", "double")
  //  val TemperatureType = new StructType().add("STN", "string").add("WBAN", "string").add("month", "int").add("day", "int").add("temperature", "double")

  def fsPath(resource: String): String =
    Paths.get(getClass.getResource(resource).toURI).toString

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    //    def parseStations(lines: RDD[String]): RDD[((String, String), Station)] =
    //      lines.map(_.split(",", -1)).filter(s => s.apply(2).nonEmpty && s.apply(3).nonEmpty).
    //        map(arr => ((arr(0), arr(1)), Station(arr(0), arr(1), arr(2).toDouble, arr(3).toDouble))).cache
    //
    //    def parseTemperatures(lines: RDD[String]): RDD[((String, String), Temperature)] =
    //      lines.map(_.split(",", -1)).filter(s => s.apply(2).nonEmpty && s.apply(3).nonEmpty && s.apply(4).nonEmpty).
    //        map(arr => ((arr(0), arr(1)), Temperature(arr(0), arr(1), arr(2).toInt, arr(3).toInt, (arr(4).toDouble - 32) * 5 / 9))).cache
    //    parseStations(spark.sparkContext.textFile(fsPath(stationsFile))).
    //      join(parseTemperatures(spark.sparkContext.textFile(fsPath(temperaturesFile)))).
    //      map(t => (LocalDate.of(year, t._2._2.month, t._2._2.day), Location(t._2._1.latitude, t._2._1.longitude), t._2._2.temperature)).collect

    val stationFrame = spark.read.schema(ScalaReflection.schemaFor[Station].dataType.asInstanceOf[StructType]).csv(fsPath(stationsFile)).as[Station].cache
    val temperatureFrame = spark.read.schema(ScalaReflection.schemaFor[Temperature].dataType.asInstanceOf[StructType]).csv(fsPath(temperaturesFile)).as[Temperature].cache
    stationFrame
      .join(temperatureFrame, stationFrame.apply("STN").eqNullSafe(temperatureFrame.apply("STN")) && stationFrame.apply("WBAN").eqNullSafe(temperatureFrame.apply("WBAN")))
      .select($"month", $"day", $"latitude", $"longitude", $"temperature").na.drop.filter($"temperature" =!= 9999.9)
      .rdd.map { case Row(month: Int, day: Int, latitude: Double, longitude: Double, temperature: Double) => (LocalDate.of(year, month, day), Location(latitude, longitude), (temperature - 32) * 5 / 9) }.collect
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records.groupBy(t => t._2).mapValues(t => t.map(_._3).sum / t.size)
  }

  /** Main function */
  def main(args: Array[String]): Unit = {
//    locateTemperatures(1975, "/stations.csv", "/1975_test.csv").foreach(println)
    locationYearlyAverageRecords(locateTemperatures(1975, "/stations.csv", "/1975_test.csv")).foreach(println)
  }
}
