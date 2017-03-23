package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)

  lazy val genHeapTuple = const(genHeap, genHeap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def count(h: H): Int = {
    isEmpty(h) match {
      case true => 0
      case false => 1 + count(deleteMin(h))
    }
  }

  def insertN(h: H, n: Int): H = {
    if (n > 0) {
      insertN(insert(arbInt.arbitrary.sample.get, h), n - 1)
    } else h
  }

  def delN(h: H, n: Int): H = {
    if (n > 0 && !isEmpty(h)) {
      delN(deleteMin(h), n - 1)
    } else h
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: A) =>
    findMin(insert(a, empty)) == a
  }

  property("min2") = forAll { (a: A, b: A) =>
    findMin(insert(b, insert(a, empty))) == Math.min(a, b)
  }

  property("minMeld1") = forAll { (h: H) =>
    findMin(meld(h, h)) == findMin(h)
  }

  property("minMeld2") = forAll { (h: H, h1: H) =>
    findMin(meld(h, h1)) == ord.min(findMin(h), findMin(h1))
  }

  property("meldSize1") = forAll { (h: H) =>
    count(meld(h, h)) == count(h) + count(h)
  }

  property("meldSize2") = forAll { (h: H, h1: H) =>
    count(meld(h, h1)) == count(h) + count(h1)
  }

  property("minGenHeap") = forAll(genHeap) { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("isEmpty1") = forAll { a: A =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("insert1") = forAll { (h: H) =>
    val originCnt = count(h)
    val h1 = insertN(h, 1)
    val cnt = count(h1)
    cnt == originCnt + 1
  }

  property("insert100") = forAll { (h: H) =>
    val originCnt = count(h)
    val h1 = insertN(h, 100)
    val cnt = count(h1)
    cnt == originCnt + 100
  }

  property("insert1GenHeap") = forAll(genHeap) { (h: H) =>
    val originCnt = count(h)
    val h1 = insertN(h, 1)
    val cnt = count(h1)
    cnt == originCnt + 1
  }

  property("insert100GenHeap") = forAll(genHeap) { (h: H) =>
    val originCnt = count(h)
    val h1 = insertN(h, 100)
    val cnt = count(h1)
    cnt == originCnt + 100
  }

  property("del1") = forAll { (h: H) =>
    val originCnt = count(h)
    val h1 = delN(h, 1)
    val cnt = count(h1)
    cnt == Math.max(0, originCnt - 1)
  }

  property("del100") = forAll { (h: H) =>
    val originCnt = count(h)
    val h1 = delN(h, 100)
    val cnt = count(h1)
    cnt == Math.max(0, originCnt - 100)
  }

  property("del1GenHeap") = forAll(genHeap) { (h: H) =>
    val originCnt = count(h)
    val h1 = delN(h, 1)
    val cnt = count(h1)
    cnt == Math.max(0, originCnt - 1)
  }

  property("del100GenHeap") = forAll(genHeap) { (h: H) =>
    val originCnt = count(h)
    val h1 = delN(h, 100)
    val cnt = count(h1)
    cnt == Math.max(0, originCnt - 100)
  }

  property("genOrderedMin") = forAll(genHeap) { (h: H) =>
    def checkOrdered(h: H): Boolean =
      isEmpty(h) match {
        case true => true
        case false =>
          isEmpty(deleteMin(h)) match {
            case true => true
            case false => ord.lteq(findMin(h), findMin(deleteMin(h))) && checkOrdered(deleteMin(h))
          }
      }

    checkOrdered(h)
  }

  property("minOutOfTwo") = forAll { (intList: List[Int]) =>
    def checkMinExists(h1: H, resList: List[Int]): Boolean =
      isEmpty(h1) match {
        case true => resList.isEmpty
        case false => findMin(h1) == resList.head && checkMinExists(deleteMin(h1), resList.tail)
      }
    val h1 = intList.foldRight(empty)((x, h) => insert(x, h))
    checkMinExists(h1, intList.sorted)
  }
}
