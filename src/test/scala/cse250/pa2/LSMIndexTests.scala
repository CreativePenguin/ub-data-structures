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
    assert(lsm(1) == "foo")
  }

}