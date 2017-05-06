package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  test("predictTemperature") {
    assert(Visualization.predictTemperature(List((Location(1, 2), 1.0)), Location(1, 2)) === 1.0)
    assert(Visualization.predictTemperature(List((Location(1, 2), 1.0)), Location(1, 2.5)) === 1.0)
  }

  test("interpolateColor") {
    assert(Visualization.interpolateColor(List((0.0,Color(255,0,0)), (1.0,Color(0,0,255))), 0.5) === Color(128,0,128))
    assert(Visualization.interpolateColor(List((0.0,Color(255,0,0)), (1.0,Color(0,0,255))), 0.0) === Color(255,0,0))
    assert(Visualization.interpolateColor(List((0.0,Color(255,0,0)), (1.0,Color(0,0,255))), 1.0) === Color(0,0,255))
    assert(Visualization.interpolateColor(List((-4.197620741379907,Color(255,0,0)), (20.551397371699693,Color(0,0,255))), -14.197620741379907) === Color(255,0,0))
    assert(Visualization.interpolateColor(List((-4.197620741379907,Color(255,0,0)), (20.551397371699693,Color(0,0,255))), 25) === Color(0,0,255))
  }

}
