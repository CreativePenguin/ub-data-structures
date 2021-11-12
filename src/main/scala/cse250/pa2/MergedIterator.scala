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

/**
 * MergedIterator's next operation continuously returns the lower 
 * of the two head elements of lhs and rhs.  If lhs and rhs both
 * return elements in ascending sorted order, then the MergedIterator
 * will return all of its elements in ascending sorted order.
 */
class MergedIterator[A: Ordering](
  lhs: BufferedIterator[A], 
  rhs: BufferedIterator[A]
)
  extends Iterator[A]
{
  /**
   * Buffered iterators support a `head` operation.  If we get a 
   * normal iterator, then upgrade it into a buffered iterator
   */
  def this(lhs: Iterator[A], rhs: Iterator[A]) =
    this(lhs.buffered, rhs.buffered)

  /**
   * Return true if either iterator has more elements.
   */
  def hasNext = lhs.hasNext || rhs.hasNext

  /**
   * If lhs.hasNext and rhs.hasNext, return the lower of the two
   * head elements.  If either iterator is empty, return the next
   * of the other iterator.
   */
  def next(): A = {
//    val min = Ordering[A].min(lhs.head, rhs.head) // use _ordering
//    this.lhs.next()
//    this.rhs.next()
    if(lhs.hasNext && rhs.hasNext) {
//      if(Ordering[A].tryCompare(lhs.head, rhs.head).isDefined) {
//        if(Ordering[A].compare(lhs.head, rhs.head) < 0)
//          lhs.next()
//        else
//          rhs.next()
//      } else {
//        if(lhs.head.)
//      }
      if(Ordering[A].compare(lhs.head, rhs.head) <= 0)
        lhs.next()
      else
        rhs.next()
    } else {
      if(lhs.hasNext) lhs.next() else rhs.next()
    }
  }
}


object MergedIterator
{
  /**
   * Use the merged iterator to merge two already sorted iterables
   * into a new sorted, indexed sequence.
   */
  def merge[A: Ordering](lhs: IterableOnce[A], rhs: IterableOnce[A]): IndexedSeq[A] =
    new MergedIterator(lhs.iterator, rhs.iterator).toIndexedSeq
}