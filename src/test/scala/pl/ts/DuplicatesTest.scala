package pl.ts

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner
import pl.ts.Duplicates._

import scala.util.Random
;

@RunWith(classOf[JUnitRunner])
class DuplicatesTest extends FunSuite {

  case class Person(id: Int)

  val Digits = (0 to 9).toList
  val DoubledDigits = Digits ++ Digits
  val ShuffleDoubledDigits = Random.shuffle(DoubledDigits)

  test("Nothing should be removed from unique elements") {
    assert(removeAllDuplicates(Nil).isEmpty)
    assert(removeAllDuplicates(Digits).toList.sorted === Digits)
  }

  test("All duplicates should be removed") {
    assert(removeAllDuplicates(DoubledDigits) === Nil)
    assert(removeAllDuplicates(ShuffleDoubledDigits) === Nil)
  }

  test("Only duplicates should be removed") {
    val uniques = List(10, 11, 12, 13, 14, 15)
    val all = DoubledDigits ++ uniques
    val allShuffled = Random.shuffle(all)

    assert(removeAllDuplicates(uniques).toList.sorted === uniques)
    assert(removeAllDuplicates(all).toList.sorted === uniques)
    assert(removeAllDuplicates(allShuffled).toList.sorted === uniques)
  }

  test("All duplicated people should be removed") {
    val people = List(Person(1), Person(1), Person(2), Person(3), Person(2), Person(4), Person(5), Person(0))
    val expected = List(Person(3), Person(4), Person(5), Person(0))

    val result = removeAllDuplicates(people)

    assert(result.size === expected.size)
    assert(result.toList.diff(expected) === Nil)
  }

  test("Expected duplicates should be removed") {
    val in = List(2, 3, 1, 1, 4, 0, 5, 7, 7, 9, 8, 0)
    val out = List(2, 3, 4, 5, 8, 9)
    assert(removeAllDuplicates(in).toList.sorted === out)
  }

  test("Result should contains only uniques") {
    val max = 10000
    val r = new Random
    val list = (0 to max).map(_ => new Person(r nextInt 20)).toList

    val after = removeAllDuplicates(list)

    assert(after.size === after.toSet.size)
  }

}
