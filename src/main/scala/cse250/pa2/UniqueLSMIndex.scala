/**
 * cse250.pa2.AppendOnlyLSMIndex
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

import scala.reflect.ClassTag
import scala.util.Sorting
import scala.collection.mutable

/**
 * An implementation of the Log Structured Merge Tree (LSM Index)
 * supporting only appends (no deletions).
 */
class UniqueLSMIndex[K:Ordering, V <: AnyRef](_bufferSize: Int)(implicit ktag: ClassTag[K], vtag: ClassTag[V])
{
  implicit val _ordering = new Ordering[(K, Option[V])]{
//    def tryCompare(a: (K, Option[V]), b: (K, Option[V])) = {
//      if(a._2.isEmpty || b._2.isEmpty) {
//        None
//      } else {
//        Some(compare(a, b))
//      }
//    }

    def compare(a: (K, Option[V]), b: (K, Option[V])) = {
//      if((a._2.isEmpty || b._2.isEmpty) &&
//        Ordering[K].compare(a._1, b._1) == 0) {
//        if(a._2.isEmpty)
//      } else Ordering[K].compare(a._1, b._1)
      Ordering[K].compare(a._1, b._1)
    }
  }

  /**
   * The input buffer.  Elements reside here until _buffer is
   * full, at which point they are sorted and placed into 
   * _levels.
   */
  val _buffer = new Array[(K,Option[V])](_bufferSize)

  /**
   * The number of elements of _buffer used.
   */
  var _bufferElementsUsed = 0

  /**
   * A sequence of sorted arrays of progressively growing sizes
   * In general, _levels(i).size == 2**i * _bufferSize
   */
  val _levels = mutable.ArrayBuffer[Option[IndexedSeq[(K,Option[V])]]]()

  def levelSize(level: Int): Int = (Math.pow(2, level) * _bufferSize).toInt

  /**
   * Insert a key, value pair into the LSM Index.
   * @param    key      The key of the record to be inserted
   * @param    value    The value of the record to be inserted
   * 
   * This function should run in ammortized O(log(n)+B) time
   */
  def insert(key: K, value: V): Unit =
  {
    // Check to see if the key is already present in the
    // buffer
    val oldIdx = _buffer.take(_bufferElementsUsed)
                        .indexWhere { _._1 == key }
    // If so, replace it with the last element in the buffer
    if(oldIdx >= 0) { 
      _bufferElementsUsed -= 1 
      if(_bufferElementsUsed > 0){
        _buffer(oldIdx) = _buffer(_bufferElementsUsed)
      }
    }

    // Append the key/value pair to the end of the 
    // buffer.
    _buffer(_bufferElementsUsed) = (key, Some(value))
    _bufferElementsUsed += 1

    // If the buffer is full, sort and promote the
    // buffer to level 0
    if(_bufferElementsUsed >= _bufferSize)
    {
      Sorting.quickSort(_buffer)
      promote(0, _buffer.toIndexedSeq)
      _bufferElementsUsed = 0
    }
  }

  /**
   * Delete a key from the LSM Index
   * @param   key     The key to remove
   * 
   * This function should run in ammortized O(log(n)+B) time
   */
  def delete(key: K): Unit = {
//    for(i <- _buffer.indices) {
//      if(_buffer(i)._1 == key) {
//        _bufferElementsUsed -= 1
//        _buffer(i) = _buffer(_bufferElementsUsed)
//        return
//      }
//    }
//    for(i <- _levels.indices) {
//      _levels(i) match {
//        case Some(li) =>
//          val foundIdx: Int = li.search((key, None))(_ordering).insertionPoint
//          if (li(foundIdx)._1 == key) {
//            if (li.length - 1 == foundIdx) {
//              _levels(i) = Some(li.slice(0, foundIdx))
//            } else {
//              _levels(i) = Some(li.slice(0, foundIdx) ++:
//                li.slice(foundIdx + 1, li.length))
//            }
//            return
//          }
//        case _ =>
//      }
//    }

  // Check to see if the key is already present in the
  // buffer
  val oldIdx = _buffer.take(_bufferElementsUsed)
    .indexWhere { _._1 == key }
    // If so, replace it with the last element in the buffer
    if(oldIdx >= 0) {
      _bufferElementsUsed -= 1
      if(_bufferElementsUsed > 0){
        _buffer(oldIdx) = _buffer(_bufferElementsUsed)
      }
    }
    _buffer(_bufferElementsUsed) = (key, None)
    _bufferElementsUsed += 1
    if(_bufferElementsUsed >= _bufferSize)
    {
      Sorting.quickSort(_buffer)
      promote(0, _buffer.toIndexedSeq)
      _bufferElementsUsed = 0
    }
//    insert(key, None)
  }

  /**
   * Install a new sequence at the specified level
   * @param  level          The level to install the sequence at
   * @param  layerContents  The sequence of elements to install
   */
  def promote(level: Int, layerContents: IndexedSeq[(K, Option[V])]): Unit = {
    while(_levels.length <= level) { _levels += None }
    _levels(level) match {
      case Some(li) =>
        val isOverflow = layerContents.length + li.length >= levelSize(level)
        //        val iter = MergedIterator.merge[K](
        //          layerContents.map(a => a._1), li.map(a => a._1))
        val iter = MergedIterator.merge(layerContents, li)(_ordering)
//        val iter = UniqueMergedIterator.merge[(K, Option[V])](layerContents, li)
        if(isOverflow) {
          _levels(level) = None
          promote(level + 1, iter)
        } else { _levels(level) = Some(iter) }
      case _ => _levels(level) = Some(layerContents)
    }
  }

  /**
   * Determine if the provided key is present in the LSM index
   * @param  key       The key to look for in the index
   * @return           True if the key is present
   * 
   * This function should run in O(log^2(n)) time
   */
  def contains(key: K): Boolean = {
    for(i <- 0 until _bufferElementsUsed) {
      if(_buffer(i) == null) {}
      else if(_buffer(i)._1 == key) {
        return _buffer(i)._2.isDefined
      }
    }
    for (i <- _levels.indices) {
      _levels(i) match {
        case Some(li) =>
          //          val point = li.search((key, new V))(_ordering).insertionPoint
          // li(0)._2 is random instance of type V, since V is ignored
          var foundIdx = li.search((key, li(0)._2))(_ordering).insertionPoint
          if (foundIdx >= li.size) {}
          else if (li(foundIdx)._1 == key && li(foundIdx)._2.isDefined)
            return true
          var isEmptyFirst: Option[Boolean] = None
          while (li(foundIdx)._1 == key && li(foundIdx)._2.isEmpty) {
            isEmptyFirst = Some(false)
            foundIdx -= 1
            if (li(foundIdx)._1 == key && li(foundIdx)._2.isDefined) {
              return true
            }
          }
          if(isEmptyFirst.isDefined)
            return isEmptyFirst.get
        case _ =>
      }
    }
    false
  }

  /**
   * Retrieve the value associated with the provided key from the
   * LSM index
   * @param  key       The key to look for in the index
   * @return           A sequence of values associated with the
   *                   specified key
   * 
   * This function should run in O(log^2(n)) time
   */
  def apply(key: K): Seq[V] = {
    var seq: Option[Seq[V]] = None
    for(i <- 0 until _bufferElementsUsed) {
      if(_buffer(i) == null) {}
      else if(_buffer(i)._1 == key) {
        if(_buffer(i)._2.isEmpty) {
          return Seq[V]()
        }
        return Seq[V](_buffer(i)._2.get)
      }
    }
    for(i <- _levels.indices) {
      _levels(i) match {
        case Some(li) =>
          var foundIdx = li.search((key, li(0)._2))(_ordering).insertionPoint
          try {
            // Checks to make sure there are no later inserts of a tombstone
            while (li(foundIdx)._1 == key) {
              if (li(foundIdx)._1 == key && li(foundIdx)._2.isEmpty) {
                seq = Some(Seq[V]())
              } else if (li(foundIdx)._1 == key && li(foundIdx)._2.isDefined) {
                seq = Some(Seq[V](li(foundIdx)._2.get))
              }
              foundIdx -= 1
            }
          } catch {
            case e: IndexOutOfBoundsException =>
          }
        case _ =>
      }
      if (seq.isDefined) return seq.get
    }
    if(seq.isEmpty) {
      Seq[V]()
    } else {
      seq.get
    }
  }

  /**
   * Generate a string representation of this LSM index
   */
  override def toString: String = 
    s"Buffer (${_bufferElementsUsed} elements): " + 
  _buffer.take(_bufferElementsUsed).map { _._1 }.mkString(", ") + "\n" +
    _levels.zipWithIndex
           .map { case (level, i) => 
              s"Level $i: " + (level match {
                case None => "[Unused]"
                case Some(contents) => contents.map { _._1 }.mkString(", ")
              })
            }
           .mkString("\n")

}