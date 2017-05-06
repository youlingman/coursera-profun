package observatory

import java.io.File

import Extraction._
import Interaction._
import Visualization._
import Visualization2._
import Interaction2._
import Manipulation._


object Main extends App {
  def generateImage(year: Int, zoom: Int, x: Int, y: Int, grid: (Int, Int) => Double) : Unit = {
    println(s"visualizeGrid for $zoom/$x/$y")
    visualizeGrid(grid, TemperatureColor, zoom, x, y).output(new File(s"target/temperatures/$year/$zoom/$x-$y.png"))
  }

  def genTileFromYears(years : Range): Unit = {
    println("start get temperatures")
    val temperatures = years.map(year => (year, locationYearlyAverageRecords(locateTemperatures(year, "/stations.csv", s"/$year.csv"))))
    println("start get grids")
    val grids = temperatures.map(t => (t._1, makeGrid(t._2)))
    println("generateTiles")
    generateTiles(grids, generateImage)
  }

  genTileFromYears(1975 to 1975)
}
