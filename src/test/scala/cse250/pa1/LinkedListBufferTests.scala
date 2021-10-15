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
package cse250.pa1

import cse250.objects.SolarInstallation
import org.scalatest.flatspec.AnyFlatSpec

class LinkedListBufferTests extends AnyFlatSpec {
  
  def createLinkedListBuffer(capacity: Int) = new LinkedListBuffer[SolarInstallation](capacity)

  behavior of "LinkedListBuffer"
  it should "keep a history of elements equal to its capacity" in {

    val buffer = createLinkedListBuffer(3);
    val e1, e2, e3, e4 = new SolarInstallation()

    // Iterator should start empty
    { 
      val iterator = buffer.iterator
      assert(!iterator.hasNext)
    }


    buffer.append(e1)

    // Iterator should return just the one element after one append
    {
      val iterator = buffer.iterator
      assert(iterator.hasNext)
      assert(e1 == iterator.next())
      assert(!iterator.hasNext)
    }

    buffer.append(e2)
    buffer.append(e3)

    // Iterator should return all three elements after 3 appends
    {
      val iterator = buffer.iterator
      assert(iterator.hasNext)
      assert(e1 == iterator.next())
      assert(iterator.hasNext)
      assert(e2 == iterator.next())
      assert(iterator.hasNext)
      assert(e3 == iterator.next())
      assert(!iterator.hasNext)
    }

    buffer.append(e4)

    // Appending e4 to a full iterator should displace e1
    {
      val iterator = buffer.iterator
      assert(iterator.hasNext)
      assert(e2 == iterator.next())
      assert(iterator.hasNext)
      assert(e3 == iterator.next())
      assert(iterator.hasNext)
      assert(e4 == iterator.next())
      assert(!iterator.hasNext)
    }

  }
}