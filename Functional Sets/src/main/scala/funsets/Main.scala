

object Main extends App {
  import FunSets._
  println(contains(x => true, 100))
  println(contains(singletonSet(1), 1))
  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)
  val s4 = singletonSet(4)
  val s5 = singletonSet(5)
  val s6 = singletonSet(0)
  val s7 = singletonSet(7)
  val s1000 = singletonSet(1000)
  val s = union(s1, s2)
  println(contains(s, 1))
  println(contains(s, 2))
  println(contains(s, 3))
  println(!contains(s, 3))
  val s10 = union(union(union(union(union(s1, s5), s3), s4), s7), s1000)
  val s8 = union(union(union(s1, s2), s3), s4)
  val s9 = diff(s10, _ < 5)
  val s11 = map(s10, x => 2*x)
  printSet(s10)
  printSet(s11)
}
