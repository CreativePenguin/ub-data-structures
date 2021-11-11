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
    assert(lsm.apply(1).contains("0"))
    assert(!lsm.apply(1).contains("1"))
    lsm.delete(7)
    assert(lsm.apply(7).isEmpty)
    assert(!lsm.contains(7))
  }
}
