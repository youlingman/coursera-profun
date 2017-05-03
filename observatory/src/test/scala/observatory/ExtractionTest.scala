package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  val stationsFilePath = "/stations.csv"
  val testFilePath = "/1975_test.csv"

  test("locateTemperatures") {
    assert(Extraction.locateTemperatures(1975, stationsFilePath, testFilePath).size == 20)
  }

  test("locationYearlyAverageRecords") {
    val records = Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(1975, stationsFilePath, testFilePath))
    assert(records.size == 2)
  }
}