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
    if(_levels.isEmpty) {
      _levels.insert(0, None)
    }
    _levels(level) match {
      case Some(li) =>
        val isOverflow = layerContents.length + li.length >=
          Math.pow(2, level) * _bufferSize
//        val iter = MergedIterator.merge[K](
//          layerContents.map(a => a._1), li.map(a => a._1))
        val iter = MergedIterator.merge[(K, V)](layerContents, li)
        if(isOverflow) {
          _levels.insert(level, None)
          promote(level + 1, iter)
        } else { _levels.insert(level, Some(iter)) }
      case _ => _levels.insert(level, Some(layerContents))
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
    for(i <- _levels) {
      i match {
        case Some(li) =>
          var j = 0
          while(Ordering[K].compare(key, li(j)._1) < 0) {
            if(Ordering[K].compare(key, li(j)._1) == 0) {
              return true
            }
            j += 1
          }
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
    var seq = Seq[V]()
    for(i <- _levels) {
      i match {
        case Some(li) =>
          var j = 0
          while(Ordering[K].compare(key, li(j)._1) < 0) {
            if(Ordering[K].compare(key, li(j)._1) == 0) {
              seq = li(j)._2 +: seq
            }
            j += 1
          }
      }
    }
    seq
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