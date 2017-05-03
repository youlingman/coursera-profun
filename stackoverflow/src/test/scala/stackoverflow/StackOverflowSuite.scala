package stackoverflow

import org.scalatest.{FunSuite, BeforeAndAfterAll}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import java.net.URL
import java.nio.channels.Channels
import java.io.File
import java.io.FileOutputStream

@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll {

  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

    override def langSpread = 50000

    override def kmeansKernels = 45

    override def kmeansEta: Double = 20.0D

    override def kmeansMaxIterations = 120
  }

  override def afterAll(): Unit = {
    StackOverflow.sc.stop()
  }

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  test("rawPostings") {
    val posts = testObject.rawPostings(StackOverflow.sc.textFile("E:/coursera/scala-spark-big-data/stackoverflow/src/main/resources/stackoverflow/stackoverflow_test.csv"))
    assert(posts.count == 20)
    val emptyPosts = testObject.rawPostings(StackOverflow.sc.parallelize(Nil))
    assert(emptyPosts.count == 0)
  }

  test("groupedPostings") {
    val grouped = testObject.groupedPostings(testObject.rawPostings(StackOverflow.sc.textFile("E:/coursera/scala-spark-big-data/stackoverflow/src/main/resources/stackoverflow/stackoverflow_test.csv")))
    assert(grouped.count() == 5)
    assert(grouped.collectAsMap().get(5484340).get.size == 1)
    assert(grouped.collectAsMap().get(9002525).get.size == 3)
    assert(grouped.collectAsMap().get(21984912).get.size == 1)
    assert(grouped.collectAsMap().get(28903923).get.size == 1)
    assert(grouped.collectAsMap().get(5077978).get.size == 2)
  }

  test("scoredPostings") {
    val score = testObject.scoredPostings(
      testObject.groupedPostings(
        testObject.rawPostings(
          StackOverflow.sc.textFile("E:/coursera/scala-spark-big-data/stackoverflow/src/main/resources/stackoverflow/stackoverflow_test.csv"))))
    assert(score.collect().length == 5)
    assert(score.collect().filter(_._1.id == 5484340).head._2 == 1)
    assert(score.collect().filter(_._1.id == 9002525).head._2 == 4)
    assert(score.collect().filter(_._1.id == 21984912).head._2 == 0)
    assert(score.collect().filter(_._1.id == 28903923).head._2 == 0)
    assert(score.collect().filter(_._1.id == 5077978).head._2 == 4)
  }

  test("vectorPostings") {
    val vector = testObject.vectorPostings(
      testObject.scoredPostings(
        testObject.groupedPostings(
          testObject.rawPostings(
            StackOverflow.sc.textFile("E:/coursera/scala-spark-big-data/stackoverflow/src/main/resources/stackoverflow/stackoverflow_test.csv")))))
    assert(vector.collect().length == 5)
    assert(vector.collectAsMap() ==
      Map(testObject.langs.indexOf("C#") * testObject.langSpread -> 1,
      testObject.langs.indexOf("C++") * testObject.langSpread -> 4,
      testObject.langs.indexOf("Java") * testObject.langSpread -> 0,
      testObject.langs.indexOf("PHP") * testObject.langSpread -> 0,
      testObject.langs.indexOf("Python") * testObject.langSpread -> 4))
  }


}
