package patmat

import org.scalatest.FunSuite

import patmat.Huffman._

class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val tl1 = List(('e',2),('g', 3),('z',1))
    val ol1 = List(Leaf('z',1),Leaf('e',2),Leaf('g', 3))
  }

  test("weight of a tiny tree") {
    new TestTrees {
      assert(weight(Leaf('a', 2)) === 2)
    }
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  // Singleton
  test("Singleton of Nil") {
    assert(!singleton(Nil))
  }

  test("Singleton of one leaf") {
    assert(singleton(List( Leaf('a',2) )))
  }

  test("Singleton of one fork") {
    new TestTrees {
      assert(singleton(List( t1 )))
    }
  }

  test("Singleton of two") {
    new TestTrees {
      assert(!singleton(List(t1, t2)))
    }
  }

  // Combine
  test("Combine of singleton") {
    new TestTrees {
      assert(combine(List(t1)) === List(t1))
    }
  }

  test("Combine of Nil") {
    assert(combine(Nil) === Nil)
  }

  // Times
  test("times 0") {
    assert(times(Nil) === Nil)
  }

  test("times 1") {
    assert(times(List('a','a')) === List(('a',2)))
  }

  test("makeOrderedLeafList 1") {
    new TestTrees {
      assert(makeOrderedLeafList(tl1) === ol1)
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") ===
           List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) ===
           List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) ===
           List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  // Decode

  // t1 = Fork(Leaf('a',2),
  //           Leaf('b',3))
  test("decode t1 - ab") {
    new TestTrees {
      assert(decode(t1, List(0,1)) === "ab".toList)
    }
  }
  test("decode t1 - aaab") {
    new TestTrees {
      assert(decode(t1, List(0,0,0,1)) === "aaab".toList)
    }
  }
  test("decode t1 - aaababaa") {
    new TestTrees {
      assert(decode(t1, List(0,0,0,1,0,1,0,0)) === "aaababaa".toList)
    }
  }

  // t2 = Fork(Fork(Leaf('a',2), Leaf('b',3)),
  //           Leaf('d',4))
  test("decode t2 - ab") {
    new TestTrees {
      assert(decode(t2, List(0,0,0,1)) === "ab".toList)
    }
  }
  test("decode t2 - abddb") {
    new TestTrees {
      assert(decode(t2, List(0,0,0,1,1,1,0,1)) === "abddb".toList)
    }
  }
  test("decode t2 - ddda") {
    new TestTrees {
      assert(decode(t2, List(1,1,1,0,0)) === "ddda".toList)
    }
  }

  // Encode
  test("encode t1 - a") {
    new TestTrees {
      assert(encode(t1)(List('a')) === List(0))
    }
  }
  test("encode t1 - b") {
    new TestTrees {
      assert(encode(t1)(List('b')) === List(1))
    }
  }
  test("encode t1 - ab") {
    new TestTrees {
      assert(encode(t1)("ab".toList) === List(0,1))
    }
  }
  test("encode t1 - abaaba") {
    new TestTrees {
      assert(encode(t1)("abaaba".toList) === List(0,1,0,0,1,0))
    }
  }

  // t2 = Fork(Fork(Leaf('a',2), Leaf('b',3)),
  //           Leaf('d',4))
  test("encode t2 - a") {
    new TestTrees {
      assert(encode(t2)(List('a')) === List(0,0))
    }
  }
  test("encode t2 - b") {
    new TestTrees {
      assert(encode(t2)(List('b')) === List(0,1))
    }
  }
  test("encode t2 - d") {
    new TestTrees {
      assert(encode(t2)(List('d')) === List(1))
    }
  }
  test("encode t2 - dad") {
    new TestTrees {
      assert(encode(t2)("dad".toList) === List(1,0,0,1))
    }
  }
  test("encode t2 - bad") {
    new TestTrees {
      assert(encode(t2)("bad".toList) === List(0,1,0,0,1))
    }
  }
  test("encode t2 - baddad") {
    new TestTrees {
      assert(encode(t2)("baddad".toList) === List(0,1,0,0,1,1,0,0,1))
    }
  }

  // Encode decode
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a text with frenchCode should be identity") {
    new TestTrees {
      assert(
        decode(frenchCode, encode(frenchCode)("louis".toList)) 
        === "louis".toList)
    }
  }

  // Quick encode decode
  test("decode and quick encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
