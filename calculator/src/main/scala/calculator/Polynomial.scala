package calculator

import scala.collection.immutable.HashSet

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal(Math.pow(b(), 2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    if (delta() < 0) Signal(Set())
    else if (delta() == 0) Signal(Set(-b() / 2 / a()))
    else Signal(Set(((-b()) + Math.sqrt(delta())) / 2 / a(), ((-b()) - Math.sqrt(delta())) / 2 / a()))
  }
}
