package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of a t2 tree") {
    new TestTrees {
      assert(weight(t2) === 9)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("timesacc") {
    new TestTrees {
      val freqs = times(string2Chars("darko drezga"));     
      assert(freqs.find(_._1 == 'a').get._2 == 2)
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val combined = combine(leaflist)
    assert(combined === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("createCodeTree for \"AAAAAAAABBBCDEFGH\"") {
    val resultTree = createCodeTree( string2Chars("AAAAAAAABBBCDEFGH") )
    assert(resultTree == Fork(Leaf('A',8),Fork(Fork(Fork(Leaf('G',1),Leaf('H',1),List('G', 'H'),2),Fork(Leaf('E',1),Leaf('F',1),List('E', 'F'),2),List('G', 'H', 'E', 'F'),4),Fork(Fork(Leaf('C',1),Leaf('D',1),List('C', 'D'),2),Leaf('B',3),List('C', 'D', 'B'),5),List('G', 'H', 'E', 'F', 'C', 'D', 'B'),9),List('A', 'G', 'H', 'E', 'F', 'C', 'D', 'B'),17))
  }
  
  test("createCodeTree for \"someText\"") {
    val resultTree = createCodeTree( string2Chars("someText") )
    assert(resultTree == Fork(Leaf('A',8),Fork(Fork(Fork(Leaf('G',1),Leaf('H',1),List('G', 'H'),2),Fork(Leaf('E',1),Leaf('F',1),List('E', 'F'),2),List('G', 'H', 'E', 'F'),4),Fork(Fork(Leaf('C',1),Leaf('D',1),List('C', 'D'),2),Leaf('B',3),List('C', 'D', 'B'),5),List('G', 'H', 'E', 'F', 'C', 'D', 'B'),9),List('A', 'G', 'H', 'E', 'F', 'C', 'D', 'B'),17))
  }
  
  test("decode secret") {
    val s = decodedSecret
    assert(s == string2Chars("huffmanestcool"))
  }
  
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t2, encode(t2)("bad".toList)) === "bad".toList)
    }
  }
  
  test("quick decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t2, quickEncode(t2)("bad".toList)) === "bad".toList)
    }
  }
}
