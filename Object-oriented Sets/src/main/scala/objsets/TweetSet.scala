package objsets
// This is modified based on an implementation from Dr. Martin Odersky in his Coursera course, Functional Programming in Scala
import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Two concrete subclasses of the abstract class TweetSet, Empty, which represents an empty set, and 
 * NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet), which represents a non-empty set as a binary tree rooted at elem. The tweets are 
 * indexed by their text bodies: the bodies of all tweets on the left are lexicographically smaller than elem and all bodies of elements on 
 * the right are lexicographically greater.
 * 
 * Note also that these classes are immutable: the set-theoretic operations do not modify this but return a new set. 
 * 
 * Some methods were already implemented for inspiration.
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   * 
   * For example, the following call: tweets.filter(tweet => tweet.retweet > 10) applied to a set tweets of two tweets, say, where the first
   * tweet was not retweeted and the second was retweeted 20 times returns a set containing only the second tweet.
   */
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)
  
  /**
   * This is a helper method for `filter` that propagates the accumulated tweets.
   * 
   * It takes an accumulator set which contains the ongoing result of the filtering.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`, i.e., a set that contains exactly the elements that are
   * either in this or in that, or in both.
   */
  def union(that: TweetSet): TweetSet
  
  /**
   * 
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set throws an exception of
   * type `java.util.NoSuchElementException`.
   */
  def mostRetweeted: Tweet
  
  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list has the highest retweet count.
   * 
   * While traversing a TweetSet, it's building a TweetList. Starting with an empty list Nil, find the tweet with the most tweets in the
   * input TweetSet. This tweet is removed from the TweetSet (that is, we obtain a new TweetSet that has all the tweets of the original set
   * except for the tweet that was removed), and added to the resulting list by creating a new Cons. After that, the process repeats itself,
   * but now we are searching through a TweetSet with one less tweet.
   */
    def descendingByRetweet: TweetList = {
      def loop(ts: TweetSet, tl: TweetList): TweetList = {
        if (ts.isInstanceOf[Empty]) tl
        else {
          val mr = ts.mostRetweeted
          if (tl.isEmpty) loop(ts.remove(mr), new Cons(mr, tl))
          else {
            def updateTweetList(xs: TweetList): TweetList = 
              if (xs.isEmpty) new Cons(mr, Nil) 
              else new Cons(xs.head, updateTweetList(xs.tail))
            loop(ts.remove(mr), updateTweetList(tl))
          }
        }
      }
      loop(this, Nil)
    }

  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc
    
  def union(that: TweetSet): TweetSet = that
  
  def mostRetweeted: Nothing = throw new NoSuchElementException("calling on an empty set")
    

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
  
  override def toString = "."
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

    def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
      if (p(elem)) {
        right.filterAcc(p, left.filterAcc(p, acc incl elem))

      }
      else {
        right.filterAcc(p, left.filterAcc(p, acc))

      }
    }
    
  def union(that: TweetSet): TweetSet = {
    if (that.isInstanceOf[Empty])
      this
    else
      left union (right union (that incl elem))
  }
  
  def mostRetweeted: Tweet = {

    val tweetSetLg = filter(x => x.retweets > elem.retweets)
    if (tweetSetLg.isInstanceOf[Empty])
      elem
    else
      tweetSetLg.mostRetweeted
  }
  

  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
  
  override def toString = "{" + left + elem + right + "}"
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}

/**
 * The task is to detect influential tweets in a set of tweets. The more often a tweet is re-tweeted, the more influential it is. A TweetSet is
 * provided containing several hundred tweets from tech news sites, located in the TweetReader object (file TweetReader.scala).
 * TweetReader.allTweets returns an instance of TweetSet containing a set of all available tweets.
 * 
 * Two lists of keywords are given additionally. The first list corresponds to keywords associated with Google and Android smartphones, while
 * the second list corresponds to keywords associated with Apple and iOS devices. The objective is to detect which platform has generated
 * more interest or activity from the data.
 */
object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")
  lazy val tweets = TweetReader.allTweets
  // contains all tweets that mentions one of the keywords in the google list
  lazy val googleTweets: TweetSet = tweets.filter(tw => google.exists(key => tw.text.contains(key)))
  //contains all tweets that mentions one of the keywords in the apple list
  lazy val appleTweets: TweetSet = tweets.filter(tw => apple.exists(key => tw.text.contains(key)))
  /**
   * A list of all tweets mentioning a keyword from either apple or google (union of those two TweetSets),
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = googleTweets union appleTweets descendingByRetweet
}

object Main extends App {    
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
	  println(set1.filter(tw => tw.user == "a"))  // empty
	  println(set5.filter(tw => tw.user == "a"))  // a
	  println(set5.filter(tw => tw.retweets == 20))  // a and b
	  println(set4c.union(set4d))  // a, b, c, d
	  println(set5.union(set1))  // a, b, c, d
	  println(set1.union(set5))  // a, b, c, d
	  val trends = set5.descendingByRetweet
	  println(trends.isEmpty)  // false
    println(trends.head.user == "a" || trends.head.user == "b")  // true
    
  // Print the trending tweets
 // GoogleVsApple.trending foreach println


    
}
