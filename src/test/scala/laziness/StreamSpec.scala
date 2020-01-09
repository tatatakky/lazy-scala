package laziness

import org.scalatest.FunSpec
import Stream._

class StreamSpec extends FunSpec {

  case class Num(n: Int)
  val s: Stream[Int] = Stream(1, 2, 3)
  val s2: Stream[String] = Stream("a", "a", "a", "b", "a")
  val s3: Stream[Nothing] = Empty
  val s4: Stream[Int] = Stream(4, 5, 6)

  describe("Stream - Lazy list") {

    it("headOption function works as expected.") {
      assert(s.headOption === Some(1))
    }

    it("sum function works as expected.") {
      assert(sum(s) === 6)
    }

    it("take function works as expected.") {
      assert(s.take(2).toList == List(1, 2))
    }

    it("drop function works as expected.") {
      assert(s.drop(2).toList === List(3))
    }

    it("exists function works as expected.") {
      assert(s.exists(_%2 == 0))
    }

    it("foldRight function works as expected.") {
      assert(s.foldRight(0)(_ + _) == 6)
    }

    it("forAll function works as expected.") {
      assert(!s2.forAll(_ == "a"))
    }

    it("headOption2 function works as expected.") {
      assert(s.headOption === Some(1))
      assert(s3.headOption === None)
    }

    it("map function works as expected.") {
      assert(s.map(_ * 2).toList === List(2, 4, 6))
    }

    it("filter function works as expected.") {
      assert(s.filter(_ % 2 == 1).toList === List(1, 3))
    }

    it("append function works as expected.") {
      assert(s.append(s4).toList === List(1, 2, 3, 4, 5, 6))
    }

    it("flatMap function works as expected.") {
      assert(s.flatMap(x => Stream(Num(x))).toList === List(Num(1), Num(2), Num(3)))
    }

    it("constant function works as expected.") {
      assert(constant(2).take(5).toList === List(2, 2, 2, 2, 2))
    }

    it("from function works as expected.") {
      assert( from(1).take(4).toList === List(1, 2, 3, 4))
    }

    it("fibs function works as expected.") {
      assert(fibs.take(7).toList === List(0, 1, 1, 2, 3, 5, 8))
    }

    it("fibsU function works as expected.") {
      assert(fibsU.take(7).toList === List(0, 1, 1, 2, 3, 5, 8))
    }

    it("fromU function works as expected.") {
      assert( fromU(1).take(4).toList === List(1, 2, 3, 4) )
    }

    it("constantU function works as expected.") {
      assert(constantU(2).take(5).toList === List(2, 2, 2, 2, 2))
    }

    it("onesU function works as expected.") {
      assert(onesU.take(5).toList === List(1, 1, 1, 1, 1))
    }

    it("times function works as expected.") {
      assert(times(5).toList === List(1, 2, 3, 4, 5))
    }
  }
}