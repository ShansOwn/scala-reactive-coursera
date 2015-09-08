package week1.quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  // if you insert an element into an empty heap, then find the minimum of the resulting heap,
  // you get the element back
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // If you insert any two elements into an empty heap,
  // finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("min2") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == math.min(a, b)
  }

  // If you insert any three elements into an empty heap,
  // finding the minimum of the resulting heap should get the smallest of the three elements back.
  property("min3") = forAll { (a: Int, b: Int, c: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    val h3 = insert(c, h2)
    findMin(h3) == math.min(math.min(a, b), c)
  }

  // If you insert an element into an empty heap, then delete the minimum,
  // the resulting heap should be empty.
  property("delMin1") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  // Given any heap, you should get a sorted sequence of elements when continually
  // finding and deleting minima
  property("sorted") = forAll { h: H =>
    def delete(a: H, accu: List[Int]): List[Int] = {
      if (isEmpty(a)) accu
      else {
        val min = findMin(a)
        delete(deleteMin(a), accu :+ min)
      }
    }
    val result = delete(h, Nil)
    result == result.sorted
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("meld") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == math.min(findMin(h1), findMin(h2))
  }

  // if you insert any 3 elements into an empty heap and delete the minimum,
  // the next minimum should be the second largest element
  property("delete") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(c, insert(b, insert(a, empty)))
    findMin(deleteMin(h)) == List(a, b, c).sorted.drop(1).head
  }

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}