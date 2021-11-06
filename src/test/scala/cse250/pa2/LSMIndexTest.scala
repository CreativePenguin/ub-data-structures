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
 * UBIT:
 * Person#:
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa2

import org.scalatest.flatspec.AnyFlatSpec

class LSMIndexTest extends AnyFlatSpec {

  def lsmIndex: LSMIndex[Int, String] =
    new LSMIndex(10)

  behavior of "LSMIndex"
  it should "Support appends" in {
    val lsm = lsmIndex

    lsm.insert(1, "foo")
    assert(lsm.contains(1))
    assert(lsm(1).head === "foo")
    assert(lsm._bufferElementsUsed === 1)
    lsm.insert(47, "bar")
    assert(lsm.contains(47))
    lsm.insert(3, "uber")
    lsm.insert(4, "dick")
    lsm.insert(5, "fart")
    lsm.insert(6, "drivel")
    lsm.insert(7, "criminal")
    lsm.insert(8, "corrupt")
    lsm.insert(9, "mate")
    assert(lsm._bufferElementsUsed === 9)
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
    var seq = lsmIndex.apply(1)

    assert(!seq.contains("foo"))
    lsm.insert(1, "foo")
    lsm.insert(1, "bar")
    seq = lsmIndex.apply(1)
    assert(lsmIndex.apply(1).contains("foo"))
    assert(lsmIndex.apply(1).contains("bar"))
  }
  it should "support promote()" in {
    val lsm = lsmIndex

    lsm.insert(1, "foo")
    lsm.promote(1, lsm._buffer.toIndexedSeq)
    assert(lsm._levels(1).isDefined)
    assert(lsm._bufferElementsUsed == 0)
    lsm.insert(2, "drug")
    lsm.promote(1, lsm._buffer.toIndexedSeq)
  }
  it should "MergedIterator next properly" in {

  }

}