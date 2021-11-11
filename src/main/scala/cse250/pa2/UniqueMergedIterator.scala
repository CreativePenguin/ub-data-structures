package cse250.pa2

class UniqueMergedIterator[A: Ordering](lhs: BufferedIterator[A], rhs: BufferedIterator[A])
  extends Iterator[A] {

  def this(lhs: Iterator[A], rhs: Iterator[A]) =
    this(lhs.buffered, rhs.buffered)

  def hasNext: Boolean = lhs.hasNext || rhs.hasNext

  def next(): A = {
    if (lhs.hasNext && rhs.hasNext) {
      val comparison = Ordering[A].compare(lhs.head, rhs.head)
      if(math.abs(comparison) == 2) {
        if(comparison == -2) {
          rhs.next()
          lhs.next()
        } else {
          lhs.next()
          rhs.next()
        }
      } else if (comparison < 0) {
        lhs.next()
      } else if (comparison == 0) {
        rhs.next()
        lhs.next()
      } else {
        rhs.next()
      }
    } else {
      if (lhs.hasNext) lhs.next() else rhs.next()
    }
  }
}

object UniqueMergedIterator
{
  /**
   * Use the merged iterator to merge two already sorted iterables
   * into a new sorted, indexed sequence.
   */
  def merge[A: Ordering](lhs: IterableOnce[A], rhs: IterableOnce[A]): IndexedSeq[A] =
    new UniqueMergedIterator(lhs.iterator, rhs.iterator).toIndexedSeq
}
