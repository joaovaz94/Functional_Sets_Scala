package funsets

/**
 * 2. Purely Functional Sets.
 */
trait FunSets extends FunSetsInterface {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  override type FunSet = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: FunSet, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): FunSet = 
    (x: Int) =>  x == elem


  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: FunSet, t: FunSet): FunSet = 
    (x: Int) => contains(s,x) || contains(t,x)
  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: FunSet, t: FunSet): FunSet = 
    (x: Int) => contains(s,x) && contains(t,x)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: FunSet, t: FunSet): FunSet = 
    (x: Int) => contains(s,x) && !contains(t,x)


  /**
   * Returns the subset of `s` for which `p` holds.
   *  
   *  Define the function filter which selects only the elements of a set that 
   *  are accepted by a given predicate p. The filtered elements are returned 
   *  as a new set. The signature of filter is as follows:
   */
  def filter(s: FunSet, p: Int => Boolean): FunSet = 
    (x: Int) => contains(s,x) && contains(p,x)


  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   *
   * Implement forall using linear recursion. For this, use a helper 
   * function nested in forall. Its structure is as follows (replace the ???):
   */
  def forall(s: FunSet, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a == bound) true
      else if (contains(s,a) && !contains(filter(s,p),a)) false
      else iter(a + 1)
    }
    iter(-bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: FunSet, p: Int => Boolean): Boolean = {
    if (forall(s,p)) true 
    else if (forall(filter(s,p), (x:Int)=> false)) false
    else exists(filter(s,p),p)
  }

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: FunSet, f: Int => Int): FunSet = ???
    /**
     { 
       def iter(t: FunSet, r: FunSet, x: Int): FunSet = {
         if (x == bound) r
         else if (contains(t,x)) iter(s, union(r, (y:Int) => f(x) == y) ,x+1)
         else iter(s, r, x+1)
       }
       iter(s, (x:Int)=> false, -3)
     }
     */

  /**
   * Displays the contents of a set
   */
  def toString(s: FunSet): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: FunSet): Unit = {
    println(toString(s))
  }
}

object FunSets extends FunSets
