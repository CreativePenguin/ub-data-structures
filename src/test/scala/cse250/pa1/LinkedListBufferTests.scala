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
    e1.fields("senior") = "junior"
    e2.fields("barf") = "penis"
    e3.fields("drugs") = "cool"

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

//    buffer.remove(e2)
//
//    // make sure remove displaces
//    {
//      val iterator = buffer.iterator
//      assert(iterator.hasNext)
//      assert(e1 === iterator.next())
//      assert(e3 === iterator.next())
//    }

  }

//  "LinkedListBuffer" should "let me grab it's values" in {
//    val li0 = new LinkedListBuffer[Int](3)
//    li0._buffer(0).set(3)
//    assert(li0._buffer(0).get === 3)
//  }

  behavior of "append()"
  it should "return None (isEmpty), and increase in _numstored size" in {
    val li1 = new LinkedListBuffer[Int](3)

    // "append()" should "return None (isEmpty), and increase in _numstored size" in
    assert(li1.append(0).isEmpty)
    assert(li1._tail === 0)
    assert(li1._buffer(li1._tail).get === 0)
    assert(li1._numStored === 1)
    assert(li1.append(1).isEmpty)
    assert(li1.append(2).isEmpty)
    assert(li1._buffer(1)._prev === 0)
    assert(li1._buffer(1)._next === 2)
    assert(li1._numStored === 3)
  }

  it should "properly adjust values after appending past capacity" in {
    val li1 = new LinkedListBuffer[Int](3)
    li1.append(0)
    li1.append(1)
    li1.append(2)
    // "append() past capacity size" should "properly adjust head and tail, and _numstored" in
    assert(li1.append(3) === Some(0))
    assert(li1._numStored === 3)
    assert(li1._head === 1)
    assert(li1._tail === 0)
    assert(li1._buffer(li1._head).get === 1)
//    assert(li1._buffer(0)._prev === 2)
//    assert(li1._buffer(2)._next === 0)
    assert(li1._buffer(li1._tail).get === 3)

    // "append()" should "not break for duplicates" in
    assert(li1.append(4) === Some(1))
    assert(li1._head === 2)
    assert(li1._tail === 1)
    assert(li1.append(3) === Some(2))
    assert(li1.append(3) === Some(3))
    assert(li1._buffer(li1._tail).get === 3)
    assert(li1._numStored === 3)
  }

  behavior of "remove()"
  it should "pass all requirements" in {
    val li1 = new LinkedListBuffer[Int](4)
    li1.append(0)
    li1.append(1)
    li1.append(-1)
    li1.append(1)

    // it should "return false if value not found & val doesn't change" in
    assert(!li1.remove(3))
    assert(li1._numStored === 4)
    assert(li1.apply(0) === 0)
    assert(li1.apply(1) === 1)
    assert(li1.apply(2) === -1)
    assert(li1._buffer(0).get === 0)
    assert(li1._buffer(1).get === 1)
    assert(li1._buffer(2).get === -1)

    // it should "get rid of all instances of value" in
    assert(li1.remove(1))
    assert(li1._numStored === 2)
//    assert(li1._buffer(0) !==)
    assertThrows[Exception] {
      li1.apply(2)
    }
    assert(li1.apply(0) === 0)
    assert(li1.apply(1) === -1)
    assert(li1._buffer(0).get === 0)
    assert(li1._buffer(0)._prev === -1)
    assert(li1._buffer(0)._next === 2)
    assert(li1._buffer(2)._prev === 0)
    assert(li1._buffer(2)._next === -1)

    val iterator = li1.iterator
    assert(iterator.next() === 0)
    assert(iterator.next() === -1)
    assert(!iterator.hasNext)

//      assert(li1.remove(0))
//      assert(li1._buffer(1) === 1)

//      assert(!li1.remove(0))
//      assert(li1._numStored === 0)

    assert(!li1.remove(1))
//    assert(!li1._buffer(1).isSet) fails bc his code is stoopid
//    assert(li1._numStored === 1)
  }

//  it should "not break when using Solar Whatever bc Idk whatelse to fix" in {
//    val e1, e2 = new SolarInstallation()
//    println(e1)
//    println(e2)
//    assert(e1 == e2)
////    assert(e1 !== e2)
//  }

  behavior of "countEntry()"
  it should "return the number of values" in {
    val li3 = new LinkedListBuffer[Char](5)
    assert(li3.countEntry('d') === 0)
    li3.append('a')
    li3.append('c')
    li3.append('a')
    li3.append('c')
    li3.append('a')

    assert(li3.countEntry('j') === 0)
    assert(li3.countEntry('a') === 3)
    assert(li3.countEntry('c') === 2)

    li3.append('a')
    li3.append('a')
//    assert(li3.countEntry('a') === 4)
  }

  "apply()" should "get value at index or throw exception" in {
    val li5 = new LinkedListBuffer[Char](5)
    assertThrows[Exception] {
      li5.apply(-1)
    }
    assertThrows[Exception] {
      li5.apply(3)
    }

    li5.append('j')
    assert(li5.apply(0) === 'j')
    assert(li5._buffer(0).get === 'j')
    li5.append('d')
    assert(li5.apply(1) === 'd')
    assert(li5._buffer(1).get === 'd')
    assertThrows[Exception] {
      li5.apply(6)
    }
  }

  "update()" should "change value or throw exception" in {
    val li6 = new LinkedListBuffer[Int](3)
//    assertThrows[IndexOutOfBoundsException] {
//      li6.update(-1, 3)
//    }
//    assertThrows[IndexOutOfBoundsException] {
//      li6.update(0, 0)
//    }

    li6.append(3)
    li6.update(0, 1)
    assert(li6.apply(0) === 1)
    assert(li6.remove(1))
    assertThrows[Exception] {
      li6.update(-1, 3)
    }
    assertThrows[Exception] {
      li6.update(1, 3)
    }
  }

  "length" should "return length" in {
    val li4 = new LinkedListBuffer[Boolean](5)
    assert(li4.length === 0)
    for(i <- 0 until 5) {
      li4.append(true)
      assert(li4.length === i + 1)
    }
//    assert(li4.length === 4)
    li4.append(false)
    assert(li4.length === 5)
    li4.remove(false)
    assert(li4.length === 4)
  }

  "everything" should "work when you're randomly removing shit" in {
    val li = new LinkedListBuffer[Int](3)
    li.append(0)
    li.append(1)
    li.append(2)

    assert(li.append(2) === Some(0))
    li.remove(1)
    assert(!li.remove(0), "0 should already be gone")
    val bullshit = li.append(4)
//    assert(bullshit.isEmpty, s"$bullshit program should know to append after 3")
    assert(li.apply(2) === 4, "apply should get 2nd value, not _buffer(i)")
    assert(li.append(2) === Some(2), "program should maintain head")
    assert(li.remove(2))
    assert(li.apply(0) === 4, "program should readjust after nuke")

    li.append(3)
    assert(li.length === 2)

    val iterator = li.iterator
    assert(iterator.next() === 4)
    assert(iterator.next() === 3)
    assert(li._buffer(0)._next === -1, li._buffer(0).get)
//    assert(li._buffer(2)._next === -1, li._buffer(2).isSet)
//    assert(!iterator.hasNext)

    li.update(1, -1)
    assert(li.apply(1) === -1)
    val tmp = Array(4, -1)
    var counter = 0
    for(i <- iterator) {
      assert(i === tmp.apply(counter))
      counter += 1
    }
  }

}
