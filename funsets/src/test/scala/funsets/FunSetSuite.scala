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
      assert(!contains(s1, 2), "Singleton")
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

  @Test def `union and intersect cases`: Unit = {
    new TestSets {
      val s4 = union(s1, s2)
      val s5 = union(s4, s3)
      val s6 = intersect(s4, s5)
      Assert.assertTrue(!contains(s4, 3))
      Assert.assertTrue(contains(s5, 3))
      Assert.assertTrue(contains(s6, 1))
      Assert.assertTrue(contains(s6, 2))
      Assert.assertTrue(!contains(s6, 3))
    }
  }

  @Test def `diff cases`: Unit = {
    new TestSets {
      val s4 = diff(s1, s2)
      val s5 = union(s1, s2)
      val s6 = diff(s5, s1)
      val s7 = diff(s1, s5)
      Assert.assertTrue(contains(s4, 1))
      Assert.assertTrue(!contains(s4, 2))
      Assert.assertTrue(!contains(s6, 1))
      Assert.assertTrue(contains(s6, 2))
      Assert.assertTrue(!contains(s7, 1))
      Assert.assertTrue(!contains(s7, 2))
    }
  }

  @Test def `filter cases`: Unit = {
    def p: Int => Boolean = x => x < 0
    val s1 = singletonSet(-1)
    val s2 = singletonSet(1)
    val s3 = union(s1, s2)
    val s4 = filter(s3, p)
    Assert.assertTrue(contains(s4, -1))
    Assert.assertTrue(!contains(s4, 1))
  }

  @Test def `forall cases`: Unit = {
    def p: Int => Boolean = x => x < 0
    val s1 = singletonSet(-1)
    val s2 = singletonSet(-2)
    val s3 = union(s1, s2)
    val s4 = union(s3, singletonSet(1))
    Assert.assertTrue(forall(s3, p))
    Assert.assertTrue(!forall(s4, p))
  }

  @Test def `exists cases`: Unit = {
    def p: Int => Boolean = x => x > 0
    val s1 = singletonSet(-1)
    val s2 = singletonSet(-2)
    val s3 = union(s1, s2)
    val s4 = union(s3, singletonSet(1))
    Assert.assertTrue(!exists(s3, p))
    Assert.assertTrue(exists(s4, p))
  }

  @Test def `map cases`: Unit = {
    def f: Int => Int = x => x*x
    val s1 = singletonSet(-1)
    val s2 = singletonSet(-2)
    val s3 = union(s1, s2)
    val s4 = map(s3, f)
    Assert.assertTrue(contains(s4, 1))
    Assert.assertTrue(!contains(s4, -1))
    Assert.assertTrue(contains(s4, 4))
    Assert.assertTrue(!contains(s4, -2))
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
