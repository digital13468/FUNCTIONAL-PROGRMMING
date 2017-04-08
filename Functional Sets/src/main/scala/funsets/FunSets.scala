
/**
 * Purely Functional Sets of Integers.
 * 
 * As an example to motivate the representation, consider representing the set of all negative integers, instead of list them all, one way
 * would be so say: if you give me an integer, I can tell you whether it's in the set or not: for 3, I say 'no'; for -1, I say yes.
 * 
 * Mathematically, we call the function which takes an integer as argument and returns a boolean indicating whether the given integer belongs
 * to a set, the characteristic function of the set. For example, we can characterize the set of negative integers by the characteristic
 * function (x: Int) => x < 0.
 * 
 * Therefore, we choose to represent a set by its characteristic function and define a type alias for this representation:  
 * type Set = Int => Boolean
 * 
 * Using this representation, we define a function that tests for the presence of a value in a set:
 * def contains(s: Set, elem: Int): Boolean = s(elem)
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
   * Basic Functions on Sets
   * 
   * Start by implementing basic functions on sets
   * 
   * singletonSet - creates a singleton set from one integer value: the set represents the set of the one given element. Having a way to
   * 								create singletone sets, we want to define a function that allow us to build bigger sets from smaller ones.
   * union, intersect, and diff - take two sets and return respectively, their union, intersection and differences.
   * filter - selects only the elements of a set that are accepted by a given predicate.  	
   */
  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = (x: Int) => x.==(elem)
  

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = (x: Int) => contains(s, x).==(true).||((contains(t, x).==(true)))
  
  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = (x: Int) => contains(s, x).==(true).&&(contains(t, x).==(true))
  
  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = (x: Int) => contains(s, x).==(true).&&(contains(t, x).==(false))
  
  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => contains(s, x).==(true).&&(p(x).==(true)) 
  
  /**
   * Queries and Transformation on Sets
   * 
   * We are interested in functions used to make requests on elements of a set. The first function tests whether a given predicae is true for
   * all elements of the set. This forall function has the following signature:
   * def forall(s: Set, p: Int => Boolean): Boolean
   * 
   * Note that there is no direct way to find which elements are in a set. the function contains only allows to know whether a given elemetn is included.
   * Thus, if we wish to do something to all elements of a set, then we have to iterate over all integers, testing each time whether it is
   * included in the set, and if so, to do something with it. Here we consider that an integer x has the property -1000 <= x <= 1000 in 
   * order to limit the search space.
   * 
   * exists - tests whether a set contains at least one element for which the given predicate is true.
   * map - transforms a given set into another one by applying to each of its element the given function.
   */
  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a < -bound) true
      else if (contains(s, a).==(true).&&(p(a).!=(true))) false
      else iter(a - 1)
    }
    iter(bound)
  }
  
  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, !p(_))
  
  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = {
      (x: Int) => exists(s, y => f(y).==(x))
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
