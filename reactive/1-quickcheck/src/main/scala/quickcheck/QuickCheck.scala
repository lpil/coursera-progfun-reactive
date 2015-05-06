package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("findMin of 2 is smallest") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == Math.min(a, b)
  }

  property("heap of 1 is empty after deleteMin") = forAll { (a: Int) =>
    val h1 = insert(a, empty)
    val h0 = deleteMin(h1)
    isEmpty(h0)
  }

  property("min of 2 melded heaps is min of one") = forAll { (a: H, b: H) =>
    val ab = meld(a, b)
    findMin(ab) == Math.min(findMin(a), findMin(b))
  }

  property("order of meld args unimportant") = forAll { (a: H, b: H) =>
    val ab = meld(a, b)
    val ba = meld(b, a)
    heapToList(ab) == heapToList(ba)
  }

  property("popping mins results in ordered list") = forAll { (h: H) =>
    val xs = heapToList(h)
    xs == xs.sorted
  }

  property("min is min again if inserted") = forAll { (h: H) =>
    val x = findMin(h)
    x == findMin(insert(x, deleteMin(h)))
  }

  property("associativity of meld") = forAll { (h1: H, h2: H, h3: H) =>
    val a = meld(meld(h1, h2), h3)
    val b = meld(meld(h3, h1), h2)
    heapToList(a) == heapToList(b)
  }



  def equalHeaps(a: H, b: H): Boolean = {
    if (isEmpty(a) && isEmpty(b))
      true
    else
      findMin(a) == findMin(b) && equalHeaps(deleteMin(a), deleteMin(b))
  }

  def heapToList(h: H): List[Int] = {
    def loop(h: H, xs: List[Int]): List[Int] = {
      if (isEmpty(h))
        xs
      else
        loop(deleteMin(h), xs :+ findMin(h))
    }
    loop(h, List())
  }


  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency((1, Gen.const(empty)), (9, genHeap))
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
