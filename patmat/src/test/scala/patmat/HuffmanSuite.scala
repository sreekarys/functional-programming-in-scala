package patmat

import org.junit._
import org.junit.Assert.assertEquals

class HuffmanSuite {
  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }


  @Test def `weight of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(5, weight(t1))
    }


  @Test def `chars of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(List('a','b','d'), chars(t2))
    }

  @Test def `string2chars hello world`: Unit =
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'), string2Chars("hello, world"))

  @Test def `times on empty list`: Unit = {
    assertEquals(0, times(List()).size)
  }

  @Test def `times on valid list`: Unit = {
    val result: List[(Char, Int)] = times(List('a', 'c' ,'a', 'c', 'b', 'a'))
    result.foreach(pair => pair._1 match {
      case 'a' => assertEquals(3, pair._2)
      case 'b' => assertEquals(1, pair._2)
      case 'c' => assertEquals(2, pair._2)
    })
  }

  @Test def `makeOrderedList and times on valid list`: Unit = {
    assertEquals(List(Leaf('b', 1), Leaf('c', 2), Leaf('a', 3)), makeOrderedLeafList(times(List('a', 'c' ,'a', 'c', 'b', 'a'))))
  }

  @Test def `make ordered leaf list for some frequency table (15pts)`: Unit =
    assertEquals(List(Leaf('e',1), Leaf('t',2), Leaf('x',3)), makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))


  @Test def `combine of some leaf list (15pts)`: Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)), combine(leaflist))
  }

  @Test def `until on a valid list of chars`: Unit = {
    assertEquals(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7),
      createCodeTree(List('x', 't', 'e', 'x', 'x', 't', 'x')))
  }

  @Test def `decode a valid tree`: Unit = {
    val root: CodeTree = createCodeTree(List('x', 't', 'e', 'x', 'x', 't', 'x'))
    assertEquals(List('x'), decode(root, List(1)))
    assertEquals(List('t'), decode(root, List(0, 1)))
    assertEquals(List('e'), decode(root, List(0, 0)))
    assertEquals(List('x', 'x'), decode(root, List(1, 1)))
    assertEquals(List('x', 'x', 'x'), decode(root, List(1, 1, 1)))
    assertEquals(List('x', 'x', 'x', 't', 'e', 't'), decode(root, List(1, 1, 1, 0, 1, 0, 0, 0, 1)))
  }

  @Test def `encode a valid tree`: Unit = {
    val root: CodeTree = createCodeTree(List('x', 't', 'e', 'x', 'x', 't', 'x'))
    assertEquals(List(1), encode(root) (List('x')))
    assertEquals(List(0 ,1), encode(root) (List('t')))
    assertEquals(List(0, 0), encode(root) (List('e')))
    assertEquals(List(1, 1), encode(root) (List('x', 'x')))
    assertEquals(List(1, 1, 1), encode(root) (List('x', 'x', 'x')))
    assertEquals(List(1, 1, 1, 0, 1, 0, 0, 0, 1), encode(root) (List('x', 'x', 'x', 't', 'e', 't')))
  }

  @Test def `decode and encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("ab".toList, decode(t1, encode(t1)("ab".toList)))
      assertEquals("ab".toList, decode(t1, quickEncode(t1)("ab".toList)))
      assertEquals("abaabaaabb".toList, decode(t1, encode(t1)("abaabaaabb".toList)))
      assertEquals("abaabaaabb".toList, decode(t1, quickEncode(t1)("abaabaaabb".toList)))
      assertEquals("aabbbddbabdbadb".toList, decode(t2, encode(t2)("aabbbddbabdbadb".toList)))
      assertEquals("aabbbddbabdbadb".toList, decode(t2, quickEncode(t2)("aabbbddbabdbadb".toList)))
    }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
