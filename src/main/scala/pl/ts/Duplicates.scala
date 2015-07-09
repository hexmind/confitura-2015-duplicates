package pl.ts

import scala.collection.mutable.Builder
import scala.collection.{mutable, immutable}

object Duplicates {

  /**
   *
   * important: Function detect and remove ALL duplicated elements.
   * It means that output doesn't contains any element duplicated in input.
   * Uniqueness is determined by object identity.
   *
   * example:
   * (1,2,2) -> (1)
   *
   * benefits:
   * - simplicty
   * - clarity
   * - expansibility
   *
   * drawbacks:
   * - memory usage (it store input with boolean uniq)
   * - simplicty
   * - linear complexity
   *
   * complexity ~ o(2n):
   * - o(1) Map.empty
   * - o(n) foldLeft
   * - o(1') contains
   * - o(1') updated
   * - o(n) for
   *
   * o(1') - constant time,that might depend on length or distribution of hash keys
   *
   * @param elements duplicated
   * @tparam T element type
   * @return only uniques
   */
  def removeAllDuplicates[T](elements: List[T]) = {
    val empty = Map.empty[T, Boolean]
    val map = elements.foldLeft(empty)((acc, e) => {
      val uniq = !acc.contains(e)
      acc.updated(e, uniq)
    })
    elements.groupBy(identity)
    for ((k, v) <- map if v) yield k
  }

}
