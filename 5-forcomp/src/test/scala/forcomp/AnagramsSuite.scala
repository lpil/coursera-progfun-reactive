package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  //
  // wordOccurrences
  //
  test("wordOccurrences: abcd") {
    assert(
      wordOccurrences("abcd") ===
      List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }
  test("wordOccurrences: Robert") {
    assert(
      wordOccurrences("Robert") ===
      List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }
  test("wordOccurrences: RobertRobert") {
    assert(
      wordOccurrences("RobertRobert") ===
      List(('b', 2), ('e', 2), ('o', 2), ('r', 4), ('t', 2)))
  }
  test("wordOccurrences: zzzzzzzzzzggggggggggU") {
    assert(
      wordOccurrences("zzzzzzzzzzggggggggggU") ===
      List(('g', 10), ('u', 1), ('z', 10)))
  }

  //
  // sentenceOccurrences
  //
  test("sentenceOccurrences: abcd e") {
    assert(
      sentenceOccurrences(List("abcd", "e")) ===
      List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }
  test("sentenceOccurrences: Robert Robert") {
    assert(
      sentenceOccurrences(List("Robert", "Robert")) ===
      List(('b', 2), ('e', 2), ('o', 2), ('r', 4), ('t', 2)))
  }
  test("sentenceOccurrences: a b c d e f f e d c b a") {
    assert(
      sentenceOccurrences(
        List("a", "b", "c", "d", "e", "f", "f", "e", "d", "c", "b", "a")) ===
      List(('a', 2), ('b', 2), ('c', 2), ('d', 2), ('e', 2), ('f', 2)))
  }
  test("sentenceOccurrences: foobar") {
    assert(
      sentenceOccurrences(
        List("foobar")) ===
      List(('a', 1), ('b', 1), ('f', 1), ('o', 2), ('r', 1)))
  }

  //
  // dictionaryByOccurrences
  //
  test("dictionaryByOccurrences.get: eat") {
    assert(
      dictionaryByOccurrences.get(
        List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) ===
      Some(Set("ate", "eat", "tea")))
  }
  test("dictionaryByOccurrences.get: bat") {
    assert(
      dictionaryByOccurrences.get(
        List(('a', 1), ('b', 1), ('t', 1))).map(_.toSet) ===
      Some(Set("bat", "tab")))
  }

  //
  // wordAnagrams
  //
  test("word anagrams: married") {
    assert(
      wordAnagrams("married").toSet ===
      Set("married", "admirer"))
  }
  test("word anagrams: player") {
    assert(
      wordAnagrams("player").toSet ===
      Set("parley", "pearly", "player", "replay"))
  }
  test("word anagrams: bat") {
    assert(
      wordAnagrams("bat").toSet ===
      Set("bat", "tab"))
  }
  test("word anagrams: eat") {
    assert(
      wordAnagrams("eat").toSet ===
      Set("eat", "ate", "tea"))
  }

  //
  // combinations
  //
  test("combinations: List()") {
    assert(
      combinations(List()) ===
      List( List() ))
  }
  test("combinations: []") {
    assert(
      combinations(Nil) ===
      List(Nil))
  }
  test("combinations: hh") {
    assert(
      combinations(List(('h', 2))) ===
      List(
        List(),
        List(('h', 1)),
        List(('h', 2))
      ))
  }
  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(
      combinations(abba).toSet ===
      abbacomb.toSet)
  }

  //
  // subtract
  //
  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(
      subtract(lard, r) ===
      lad)
  }
  test("subtract: larrd - r") {
    val larrd = List(('a', 1), ('d', 1), ('l', 1), ('r', 2))
    val r = List(('r', 1))
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    assert(
      subtract(larrd, r) ===
      lard)
  }
  test("subtract: larrd - larrd") {
    val larrd = List(('a', 1), ('d', 1), ('l', 1), ('r', 2))
    assert(
      subtract(larrd, larrd) ===
      Nil)
  }
  test("subtract: louis - lou") {
    val louis = List(('i', 1), ('l', 1), ('o', 1), ('s', 1), ('u', 1))
    val lou   = List(('l', 1), ('o', 1), ('u', 1))
    val    is = List(('i', 1), ('s', 1))
    assert(
      subtract(louis, lou) ===
      is)
  }

  //
  // sentenceAnagrams
  //
  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }
  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(
      sentenceAnagrams(sentence).toSet ===
      anas.toSet)
  }  

}
