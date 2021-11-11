/**
 * cse250.pa1.LinkedListBufferTests
 *
 * Copyright 2021 Oliver Kennedy (okennedy@buffalo.edu)
 *           2021 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: Winston Peng
 * Person#: 5038646
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa2

import org.scalatest.flatspec.AnyFlatSpec

class LSMIndexTest extends AnyFlatSpec {

  def lsmIndex: LSMIndex[Int, String] =
    new LSMIndex(100)

  behavior of "LSMIndex"
//  it should "have working levelSize" in {
//    val lsm = lsmIndex
//    assert(lsm.levelSize(0) === 100)
//    assert(lsm.levelSize(1) === 200)
//    assert(lsm.levelSize(4) === 1600)
//  }
  it should "Support appends" in {
    val lsm = lsmIndex

    lsm.insert(1, "foo")
    assert(lsm._bufferElementsUsed === 1)
    lsm.insert(47, "bar")
//    assert(lsm.contains(47))
    lsm.insert(3, "uber")
    lsm.insert(4, "butt")
    lsm.insert(5, "fart")
    lsm.insert(6, "drivel")
    lsm.insert(7, "criminal")
    lsm.insert(8, "corrupt")
    lsm.insert(9, "mate")
    assert(lsm._bufferElementsUsed === 9)
    for(i <- 0 until 100) {
      lsm.insert(i, i.toString)
    }
    assert(lsm.contains(45))
    assert(lsm.contains(1))
    assert(lsm.apply(1).contains("1"))
    assert(lsm.apply(1).contains("foo"), lsm.apply(1))
//    print(lsm)
//    print(lsm)
//    assert(lsm(1).head === "foo", lsm)
    assert(lsm.contains(99))
    assert(!lsm.contains(199))
    assert(lsm._bufferElementsUsed < 100)
    assert(lsm._levels(0).isDefined)
    assert(lsm._levels(0).get.length === 100, lsm._levels(0))
    for(i <- 0 until 100) {
      lsm.insert(i + 200, i.toString)
    }
    assert(lsm._levels(0).isEmpty)
    assert(lsm._levels(1).isDefined)
    assert(lsm._levels(1).get.length === 200)
    assert(lsm._levels.length === 2, lsm)
    var prev = lsm._levels(1).get(0)
    for(i <- lsm._levels(1).get.slice(1, 200)) {
      assert(lsm._ordering.compare(prev, i) <= 0)
      prev = i
    }
    for(i <- 0 until 200) {
      lsm.insert(i + 400, i.toString)
    }
    assert(lsm._levels(0).isEmpty)
    assert(lsm._levels(1).isEmpty)
    assert(lsm.apply(410).contains("10"))
//    print(lsm)
//    assert(lsm._buffer.length === 0)
  }
  it should "Not fail contains" in {
    val lsm = lsmIndex

    assert(!lsm.contains(1))
    lsm.insert(1, "foo")
    assert(lsm.contains(1))
  }
  it should "support apply()" in {
    val lsm = lsmIndex
    var seq = lsm

    assert(!seq.contains(1))
    lsm.insert(1, "dooda")
    lsm.insert(1, "foo")
    assert(lsm.apply(1).contains("dooda"))
    assert(lsm.apply(1).contains("foo"))
//    lsm.promote(0, lsm._buffer.toIndexedSeq)
//    seq = lsm.apply(1)
    lsm.insert(1, "bar")
    assert(lsm.apply(1).contains("bar"))
    assert(lsm.apply(1).contains("foo"))
  }
//  it should "support promote()" in {
//    val lsm = lsmIndex
//
//    lsm.insert(1, "foo")
//    lsm.promote(0, lsm._buffer.toIndexedSeq)
//    assert(lsm._levels(1).isDefined)
//    assert(lsm._bufferElementsUsed === 0)
//    lsm.insert(2, "drug")
//    lsm.promote(0, lsm._buffer.toIndexedSeq)
//  }
//  it should "MergedIterator next properly" in {
//
//  }

}