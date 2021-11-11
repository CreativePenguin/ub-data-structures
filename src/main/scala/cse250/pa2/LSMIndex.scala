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

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Sorting
import scala.collection.mutable

/**
 * An implementation of the Log Structured Merge Tree (LSM Index)
 * supporting only appends (no deletions).
 */
class LSMIndex[K:Ordering, V <: AnyRef](_bufferSize: Int)(implicit ktag: ClassTag[K], vtag: ClassTag[V])
{
  implicit val _ordering = new Ordering[(K, V)]{
    def compare(a: (K, V), b: (K, V)) =
      Ordering[K].compare(a._1, b._1)
  }

  /**
   * The input buffer.  Elements reside here until _buffer is
   * full, at which point they are sorted and placed into 
   * _levels.
   */
  val _buffer = new Array[(K,V)](_bufferSize)

  /**
   * The number of elements of _buffer used.
   */
  var _bufferElementsUsed = 0

  /**
   * A sequence of sorted arrays of progressively growing sizes
   * In general, _levels(i).size == 2**i * _bufferSize
   */
  val _levels = mutable.ArrayBuffer[Option[IndexedSeq[(K,V)]]]()

  def levelSize(level: Int): Int = (Math.pow(2, level) * _bufferSize).toInt

  /**
   * Insert a key, value pair into the LSM Index.
   * @param    key      The key of the record to be inserted
   * @param    value    The value of the record to be inserted
   * 
   * This function should run in ammortized O(log(n)) time
   */
  def insert(key:K, value:V): Unit =
  {
    // Append the key/value pair to the end of the 
    // buffer.
    _buffer(_bufferElementsUsed) = (key, value)
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
   * Install a new sequence at the specified level
   * @param  level          The level to install the sequence at
   * @param  layerContents  The sequence of elements to install at the layer
   */
  def promote(level: Int, layerContents: IndexedSeq[(K, V)]): Unit = {
    while(_levels.length <= level) { _levels += None }
    _levels(level) match {
      case Some(li) =>
        val isOverflow = layerContents.length + li.length >= levelSize(level)
//        val iter = MergedIterator.merge[K](
//          layerContents.map(a => a._1), li.map(a => a._1))
        val iter = MergedIterator.merge[(K, V)](layerContents, li)
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
      else if(_buffer(i)._1 == key) return true
    }
    for (i <- _levels.indices) {
      _levels(i) match {
        case Some(li) =>
//          val point = li.search((key, new V))(_ordering).insertionPoint
          // li(0)._2 is random instance of type V, since V is ignored
          val foundIdx = li.search((key, li(0)._2))(_ordering).insertionPoint
          if(foundIdx >= li.size) {}
          else if (li(foundIdx)._1 == key) return true
        case _ =>
      }
    }
    false
  }

  /**
   * Helper function to implement binary search in apply()
 *
   * @param key same key as in apply()
   * @param seq current half of _level to search through
   */
  def applyH(key: K, seq: IndexedSeq[(K, V)]): Seq[V] = {
    if(seq.length == 1) {
      if(seq(0)._1 == key) { seq.map(x => x._2) }
      else { Seq() }
    } else {
      val midComparison = Ordering[K].compare(key, seq(seq.length / 2)._1)
      if(midComparison < 0) {
        applyH(key, seq.slice(0, seq.length / 2))
      } else if (midComparison > 0) {
        applyH(key, seq.slice(seq.length / 2, seq.length))
      } else {
        var i = seq.length / 2
        var retVal = Seq[V]()
        while(seq(i)._1 == key) {
          retVal = seq(i)._2 +: retVal
          i -= 1
        }
        i = seq.length / 2 + 1
        while(seq(i)._1 == key) {
          retVal = seq(i)._2 +: retVal
          i += 1
        }
        retVal
      }
    }
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
    var seq = Seq[V]()
    for(i <- _buffer) {
      if(i == null) {}
      else if(i._1 == key) seq = seq :+ i._2
    }
    for(i <- _levels) {
      i match {
        case Some(li) =>
//          seq = seq ++: applyH(key, li)
          val foundIdx = li.search((key, li(0)._2))(_ordering).insertionPoint
          var foundIdxCpy = foundIdx
          // Check surrounding vars for duplicates
          try {
            while(li(foundIdxCpy)._1 == key) {
              seq = seq :+ li(foundIdxCpy)._2
              foundIdxCpy -= 1
            }
            foundIdxCpy = foundIdx + 1
            while(li(foundIdxCpy)._1 == key) {
              seq = seq :+ li(foundIdxCpy)._2
              foundIdxCpy += 1
            }
          } catch {
            case e: IndexOutOfBoundsException =>
          }

//          if (li(foundIdx)._1 == key) {
//            seq = seq :+ li(foundIdx)._2
//          }
        case _ =>
      }
    }
    seq
  }

  /**
   * Generate a string representation of this LSM index
   */
  override def toString: String = {
    s"Buffer (${_bufferElementsUsed} elements): " +
      _buffer.take(_bufferElementsUsed).map {
        _._1
      }.mkString(", ") + "\n" +
      _levels.zipWithIndex
        .map { case (level, i) =>
          s"Level $i: " + (level match {
            case None => "[Unused]"
            case Some(contents) => contents.map {
              _._1
            }.mkString(", ")
          })
        }
        .mkString("\n")
  }
}