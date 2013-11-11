package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import org.scalacheck.util.Buildable
import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("empty") = forAll { a: Int =>
    isEmpty(empty)
  }
  property("isEmpty") = forAll { a: Int =>
    isEmpty(empty) && isEmpty(deleteMin(insert(a, empty)))
  }
  property("insert") = forAll { (a: Int, heap: H) =>
    val h1 = if (isEmpty(heap)) insert(a, heap) else heap
    val min = findMin(h1)
    findMin(insert(min, deleteMin(h1))) == min
  }
  property("meld") = forAll { (heap1: H, heap2: H) =>

    def hasSameMin(a: H, b: H) = {
      (!isEmpty(a) && !isEmpty(b)) && findMin(a) == findMin(b)
    }

    @tailrec
    def down(h: H, h1: H, h2: H): Boolean = {
      if (isEmpty(h) && isEmpty(h1) && isEmpty(h2)) true
      else if (hasSameMin(h, h1)) down(deleteMin(h), deleteMin(h1), h2)
      else if (hasSameMin(h, h2)) down(deleteMin(h), h1, deleteMin(h2))
      else false
    }

    down(meld(heap1, heap2), heap1, heap2)

  }
  property("findMin") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val min = if (a > b) b else a
    findMin(h) == min
  }

  property("deleteMin") = forAll { heap: H =>
    (!isEmpty(heap)) ==> {

      @tailrec
      def delMin(h: H): (H, Int) = {
        val min = findMin(h)
        val h1 = deleteMin(h)
        lazy val min2 = findMin(h1)
        if (isEmpty(h1) || min != min2) (h1, min)
        else delMin(h1)
      }
      val (h, min) = delMin(heap)
      isEmpty(h) || findMin(h) != min
    }
  }

  lazy val genHeap: Gen[H] =
    for {
      elements <- listOf(arbitrary[Int])
    } yield (elements.foldRight(empty)((el, h) => insert(el, h)))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
