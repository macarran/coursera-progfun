package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(2), 3))
  
  val setOf2 = singletonSet(2)
  val setOf3 = singletonSet(3)
  val setOf4 = singletonSet(4)
  val setOf5 = singletonSet(5)
  val setOf6 = singletonSet(6)
  

  val set_2_4_6 = union(union(setOf2, setOf4), setOf6) 
  val set_2_3_5 = union(union(setOf2, setOf3), setOf5)
  
  printSet(set_2_3_5)
  printSet(set_2_4_6)
  
  println(contains(set_2_4_6, 4) + " should be true")
  println(contains(set_2_3_5, 4) + " should be false")
  
  println(contains(intersect(set_2_4_6, set_2_3_5), 2) + " should be true")
  println(contains(intersect(set_2_4_6, set_2_3_5), 4) + " should be false")
  println(contains(intersect(set_2_4_6, set_2_3_5), 5) + " should be false")
  
  println(contains(diff(set_2_4_6, set_2_3_5), 4) + " should be true")
  println(contains(diff(set_2_4_6, set_2_3_5), 5) + " should be false")
  
  println(forall(set_2_3_5, x => x > 0) + " should be true")
  println(forall(set_2_3_5, x => x < 10) + " should be true")
  println(forall(set_2_3_5, x => x > 2) + " should be false")
  
  val emptySet = intersect(setOf2, setOf3);
  println(forall(emptySet, x => x < 0) + " should be true?")
  
  println(exists(set_2_3_5, x => x > 0) + " should be true")
  println(exists(set_2_3_5, x => x < 3) + " should be true")
  println(exists(set_2_3_5, x => x > 2) + " should be true")
  println(exists(set_2_3_5, x => x > 5) + " should be false")
  
  printSet(map(set_2_3_5, x => x * 2))
}
