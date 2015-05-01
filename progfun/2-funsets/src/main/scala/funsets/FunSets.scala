package funsets

import common._

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set =
    (x: Int) => x == elem

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set =
    (x: Int) => s(x) || t(x)


  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set =
    (x: Int) => s(x) && t(x)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set =
    (x: Int) => s(x) && !t(x)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set =
    (x: Int) => s(x) && p(x)

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(set: Set, pred: Int => Boolean): Boolean = {
    def loop(a: Int): Boolean = {
      if (a == bound + 1) true
      // Return false if a num is in the set but doesn't satisfy the pred
      else if (set(a) && !pred(a)) false
      else loop(a + 1)
    }
    loop(-bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(set: Set, pred: Int => Boolean): Boolean =
    !forall(set, (x: Int) => !pred(x))

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(set: Set, f: Int => Int): Set = {
    def loop(a: Int, acc: List[Int]): List[Int] = {
      if (a == bound + 1) acc
      else if (set(a)) loop(a + 1, f(a) :: acc)
      else loop(a + 1, acc)
    }
    (x: Int) => loop(-bound, List()).contains(x)
  }

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
