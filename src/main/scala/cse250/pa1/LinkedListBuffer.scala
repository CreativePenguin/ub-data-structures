/**
 * cse250.pa1.LinkedListBuffer
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

class LinkedListBuffer[A](capacity: Int)
  extends scala.collection.mutable.Seq[A]
{
  val _buffer = Array.fill[LinkedListNode](capacity) { new LinkedListNode(None) }
  val _removed = new FakeQueue(capacity)
  var _numStored = 0
  var _head = -1
  var _tail = -1

  /**
   * Append an entry into the sequence, displacing the oldest entry if needed.
   * @param     entry       The entry to insert
   * @return                The entry that was displaced, or None
   * 
   * After this function is called, `entry` should be the final element of the 
   * sequence.  If adding `entry` would cause the size of the sequence to 
   * exceed its `capacity`, the first entry in the sequence should be removed.
   * 
   * Data may only be stored in `_buffer`.  Your solution may not use any other
   * collection types.
   * 
   * This function must run in O(n) time, where n = [[length]].
   * 
   * 2 bonus points will be awarded if this function runs in Θ(1) time 
   * 
   * (assume that all times are non-amortized unless otherwise specified).
   */
  def append(entry: A): Option[A] = {
    if(_numStored < capacity) {
      _head = Math.max(0, _head)
      val prevtail = _tail
      if(_removed.length > 0) {
        _tail = _removed.dequeue()
      } else {
        _tail += 1
      }
      _buffer(_tail).set(entry)
      _buffer(_head)._prev = -1
      _buffer(_tail)
      if(_numStored != 0) {
        _buffer(prevtail)._next = _tail
        _buffer(_tail)._prev = prevtail
      }
      _numStored += 1
      None
    } else {
      val removedVal = _buffer(_head).get
      _buffer(_tail)._next = _head
      _buffer(_head).set(entry)
      _buffer(_head)._prev = _tail
      _tail = _head
      _head = _buffer(_head)._next
      _buffer(_head)._prev = -1
      _buffer(_tail)._next = -1
      Some(removedVal)
    }
  }

  /**
   * Remove all instances of an element from the sequence.
   * @param     entry       The entry to remove.
   * @return                True if at least one instance of the entry was 
   *                        removed.
   * 
   * After this function is called, the sequence should contain no elements
   * that are equal (according to `==`) to `entry`.
   * 
   * This function must run in O(n) time, where n = [[length]] 
   */
  def remove(entry: A): Boolean = {
    if(_numStored == 0) { return false }
    var i = _head
    var containsEntry = false
    while(i != -1) {
      if(_buffer(i).get == entry) {
        containsEntry = true
        if(i == _head) {
          _head = _buffer(_head)._next
          if(_head != -1) { _buffer(_head)._prev = -1 }
        } else if(i == _tail) {
          _tail = _buffer(_tail)._prev
          _buffer(_tail)._next = -1
        } else {
          val prev = _buffer(_buffer(i)._prev)
          val next = _buffer(_buffer(i)._next)
          prev._next = _buffer(i)._next
          next._prev = _buffer(i)._prev
        }
        _buffer(i).clear
        _removed.enqueue(i)
//        if(i + 1 < _numStored) _buffer(i + 1)._prev = i - 1
        _numStored -= 1
      }
      i = _buffer(i)._next
    }
    containsEntry
  }
  /**
   * Return the current length of the sequence
   * @return                The number of elements in the sequence
   * 
   * Note that this is NOT exactly the same as the number of calls to append 
   * minus the number of calls to remove, since append will not increase the
   * size of the sequence beyond the sequence's `capacity`.
   * 
   * This function must run in Θ(1) time.
   */
  override def length: Int = _numStored

  /**
   * Retrieve the `idx`th element to be inserted into the sequence (i.e., the
   * `idx`th element in insertion order)
   * @param    idx      The position of the element to retrieve.
   * @return            The element at position `idx`
   * 
   * The order of the sequence is the order in which elements were inserted.
   * 
   * This function must run in O(n) time, where n = `idx`
   */
  override def apply(idx: Int): A = {
    if(idx > _numStored) {
      throw new IndexOutOfBoundsException
    }
    var i = _head
    for(j <- 0 until idx) {
      i = _buffer(i)._next
    }
    _buffer(i).get
  }

  /**
   * Count the number of times `entry` occurs in the sequence.
   * @param    entry     The entry to count in the sequence
   * @return             The number of times entry (according to ==) occurs
   * 
   * Count the number of times the specified `entry` occurs in the sequence.
   * 
   * 
   * This function must run in O(n) time, where n = [[length]] 
   */
  def countEntry(entry: A): Int = {
    var i = _head
    var count = 0
    while(i != -1) {
      if(_buffer(i).get == entry) {
        count += 1
      }
      i = _buffer(i)._next
    }
//    if(_buffer(i).get == entry) {
//      count += 1
//    }
    count
  }

  /**
   * Update the value at position `idx` to `elem`
   * @param    idx       The index to update
   * @param    elem      The element to update the sequence to
   * 
   * Modify the sequence, replacing the element previously at position `idx` 
   * with `elem`.  This does not otherwise change the order of the list.
   * 
   * This function must run in O(n) time, where n = `idx`
   */
  def update(idx: Int, elem: A): Unit = {
    if(idx < 0 || idx > _numStored - 1) {
      throw new IndexOutOfBoundsException
    }
    var i = _head
    for(j <- 0 until idx) {
      i = _buffer(i)._next
    }
    _buffer(i).set(elem)
  }

  /**
   * Return an iterator over the elements of this sequence
   * @return                An iterator over the elements of this sequence
   * 
   * Iteration should proceed in order of insertion.  The first element to be
   * [[append]]ed should be the first element the iterator visits.  The most
   * recent element to be [[append]]ed should be the last element the iterator 
   * visits
   * 
   * This function must run in Θ(1) time.
   */
  override def iterator: LinkedListIterator =
    new LinkedListIterator()

  /**
   * Render a graphical representation of the list
   */
  override def toString(): String = 
    iterator.map { "[" + _ + "]" }.mkString(" ↔ ")

  /**
   * One node of a linked list.
   */
  class LinkedListNode(var _value: Option[A])
  {
    /**
     * A reference (pointer) to the position in `_buffer` where the preceding
     * element of the sequence is located, or -1 if this is the first element
     * of the sequence.
     */
    var _prev: Int = -1

    /**
     * A reference (pointer) to the position in `_buffer` where the following
     * element of the sequence is located, or -1 if this is the last element
     * of the sequence.
     */
    var _next: Int = -1

    /**
     * Return true if the linked list node is in-use
     */
    def isSet = _value.isDefined

    /**
     * Assign a value to this node
     */
    def set(value: A) = { _value = Some(value) }

    /**
     * Clear the value in this node
     */
    def clear = { _value = None }

    /**
     * Get the current node value (if set)
     */
    def get = { _value.get }
  }

  /**
   * An iterator over the elements of this linked list.
   */
  class LinkedListIterator extends Iterator[A]
  {
    var _curr = _head

    /**
     * Return true if there are additional elements in this iterator, or 
     * false if the iterator has no further elements.
     * @return        True if next() will return another element.
     * 
     */
    override def hasNext: Boolean = { _curr > -1 }

    /**
     * Return the next element of the iterator, and advance the iterator to 
     * the following position.
     * @return        The sequentially next element from the sequence
     * 
     * This method may throw a [[NoSuchElementException]] if it is called
     * hasNext() returns false.
     */
    override def next(): A = { 
      if(_curr == -1){ throw new NoSuchElementException() }
      
      val currentElement = _buffer(_curr)
      _curr = currentElement._next
      return currentElement._value.get
    }

    /**
     * Remove the last element returned by [[next]] from the underlying list.
     * 
     * This method must throw a [[NoSuchElementException]] if it is called
     * before [[next]] is called for the first time on this iterator.
     */
    def remove(): Unit = {
      if(_curr == _head) { throw new NoSuchElementException() }
    }
  }

  class FakeQueue(capacity: Int) {
    val _buffer = Array.fill[Int](capacity) { -1 }
    var _head = -1
    var _tail = -1
    var _numStored = 0

    def enqueue(entry: Int): Unit = {
      if(_numStored < capacity) {
        _head = Math.max(0, _head)
        _tail = (_tail + 1) % capacity
        _buffer(_tail) = entry
        _numStored += 1
      } else {
        throw new IndexOutOfBoundsException()
      }
    }

    def dequeue(): Int = {
      val prevhead = _head
      _head = _head + 1
      _numStored -= 1
      _buffer(prevhead)
    }

    def length: Int = _numStored
  }
}
