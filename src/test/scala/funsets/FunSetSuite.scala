package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(-1)
    val s6 = singletonSet(-2)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
   * @Ignore annotation.
   */
  @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `intersect contains only repeated values `: Unit = {
    new TestSets {
      val s = union(s1, union(s2,s3))
      val t = union(s2, union(s3,s4))
      val r = intersect(s,t)
      assert(contains(r,2), "Intersect 2")
      assert(contains(r,3), "Intersect 3")
      assert(!contains(r,1), "Intersect 1")
      assert(!contains(r,4), "Intersect 4")
    }
  }

  @Test def `diff contains only no-repeated values `: Unit = {
    new TestSets {
      val s = union(s1, union(s2,s3))
      val t = union(s3,s4)
      val r = diff(s,t)
      assert(!contains(r,4), "diff doesn't contain 2")
      assert(!contains(r,3), "diff doesn't contain  3")
      assert(contains(r,1), "diff contains 1")
      assert(contains(r,2), "diff contains 4")
    }
  }

  @Test def `filter returns set filtered by function`: Unit = {
    new TestSets {
      val s = union(s1, union(s2,union(s3,s4)))
      val t = filter(s,(x: Int) => if (x <= 2) true else false)
      assert(contains(t, 1), "Filter contains 1")
      assert(contains(t, 2), "Filter contains 2")
      assert(!contains(t, 3), "Filter doesn't contain 3")
      assert(!contains(t, 4), "Filter doesn't contain 4")
    }
  }

 @Test def `Retuns wheter all members of a set satisfeis function`: Unit = {
    new TestSets {
      val s = union(s1, union(s2,union(s3,union(s4,union(s5,s6)))))
      assert(!forall(s,(x: Int) => if (x > 0) true else false), "x > 0 is false for all set")
      assert(forall(s,(x: Int) => if (x != 0) true else false), "x != 0 is true for all set")
    }
  }

 @Test def `Retuns wheter any members of a set satisfeis function`: Unit = {
   new TestSets {
     val s = union(s1, union(s2,union(s3,union(s4,s6))))
     val t = union(s1, union(s2,union(s3,s4)))
     assert(exists(s,(x: Int) => if (x < 0) true else false), "x > 0 is true for at leats 1 member of the set")
     assert(!exists(t,(x: Int) => if (x < 0) true else false), "x > 0 is not true for any member of the set")
   }
 }

@Test def `map returns a new set based on the given function`: Unit = {
  new TestSets {
     val s = union(s1, union(s2,s3))//{1,2,3}
     val t = union(s2, union(s3,s4))//{2,3,4}
    map(s,(x:Int) => x+1)
    assert(s == t, "The set s was modified to be equal to t")
  }
}

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
