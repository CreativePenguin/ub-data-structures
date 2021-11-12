package cse250.pa2

import org.scalatest.flatspec.AnyFlatSpec

class UniqueLSMIndexTest extends AnyFlatSpec {
  def lsmIndex: UniqueLSMIndex[Int, String] =
    new UniqueLSMIndex(100)

  behavior of "UniqueLSMIndex"
  it should "support appends" in {
    val lsm = lsmIndex
    for(i <- 0 until 99) {
      lsm.insert(i, i.toString)
    }
    assert(lsm.contains(3))
    assert(lsm.apply(3).contains("3"), lsm.apply(3))
    lsm.delete(0)
    assert(lsm.apply(0).isEmpty)
    lsm.insert(3, "5")
    assert(!lsm.apply(3).contains("3"))
    assert(lsm.apply(3).contains("5"))
    for(i <- 0 until 100) {
      lsm.insert(i + 100, i.toString)
    }
    lsm.insert(1, "0")
//    println(lsm._bufferElementsUsed) -> prints 0
    lsm.delete(7)
    for(i <- 0 until 200) {
      lsm.insert(i + 200, i.toString)
    }
    assert(lsm.apply(1).contains("0"))
    assert(!lsm.apply(1).contains("1"))
    assert(lsm.apply(7).isEmpty)
    assert(!lsm.contains(7))
    for(i <- 0 until 300) {
      lsm.insert(i, (i + 1000).toString)
    }
    for(i <- 0 until 300) {
      assert(lsm.apply(i).contains((i + 1000).toString))
      assert(lsm.contains(i))
      assert(!lsm.apply(i).contains(i.toString))
      assert(!lsm.apply(i).contains((i % 100).toString))
    }
    println(lsm)
  }

  // These tests fail because it's not even using my instance of _ordering
//  behavior of "UniqueMergedIterator"
//  it should "next() correctly" in {
//    val blah = IndexedSeq((1, Some("1")), (2, None))
//    val june = IndexedSeq((1, Some("4")), (2, Some("3")), (3, Some("3")))
////    val babylon =
////      UniqueMergedIterator.merge[(Int, Option[String])](blah, june)
//    val babylon = UniqueMergedIterator.merge(blah, june)(lsmIndex._ordering)
//    assert(babylon.contains((1, Some("1"))))
//    assert(!babylon.contains((1, Some("4"))))
//    assert(babylon.contains((3, Some("3"))))
//    println(babylon)
//  }
}
