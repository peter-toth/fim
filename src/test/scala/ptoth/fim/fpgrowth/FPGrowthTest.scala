/*
 * Copyright 2018 Peter Toth
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ptoth.fim.fpgrowth

import org.scalatest.FunSuite
import org.scalatest.Inspectors.forAll
import ptoth.fim.{CountingAccumulator, FrequentItemSet}
import ptoth.fim.FrequentItemSetUtils._

class FPGrowthTest extends FunSuite {

  test("Mining of an empty database should return nothing") {
    val frequentItemSets = FPGrowth(Array.empty[Array[String]], 1).mine()

    assert(frequentItemSets.size === 0)
  }

  test("Mining of a one element database with minFrequency = 1 should return the only element") {
    val frequentItemSets = FPGrowth(Array(Array("A")), 1).mine()

    assert(ordered(frequentItemSets.items) === ordered(List(FrequentItemSet(Array("A"), 1))))
  }

  test("Mining of a one element database with minFrequency = 2 should return nothing") {
    val frequentItemSets = FPGrowth(Array(Array("A")), 2).mine()

    assert(frequentItemSets.size === 0)
  }

  test("Mining of a 2 element database with minFrequency = 1 should return the 2 elements and the union of them") {
    val frequentItemSets = FPGrowth(Array(Array("A", "B")), 1).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(FrequentItemSet(Array("A", "B"), 1), FrequentItemSet(Array("A"), 1), FrequentItemSet(Array("B"), 1))
      )
    )
  }

  test("Mining of a 3 element database with minFrequency = 1 should return the 6 frequent itemsets") {
    val frequentItemSets = FPGrowth(Array(Array("A", "B", "C")), 1).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(
          FrequentItemSet(Array("A", "B", "C"), 1),
          FrequentItemSet(Array("A", "B"), 1),
          FrequentItemSet(Array("A", "C"), 1),
          FrequentItemSet(Array("A"), 1),
          FrequentItemSet(Array("B", "C"), 1),
          FrequentItemSet(Array("B"), 1),
          FrequentItemSet(Array("C"), 1)
        )
      )
    )
  }

  test("Mining of a 4 element database with minFrequency = 1 should return the 15 frequent itemsets") {
    val frequentItemSets = FPGrowth(Array(Array("A", "B", "C", "D")), 1).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(
          FrequentItemSet(Array("A", "B", "C", "D"), 1),
          FrequentItemSet(Array("A", "B", "C"), 1),
          FrequentItemSet(Array("A", "B", "D"), 1),
          FrequentItemSet(Array("A", "B"), 1),
          FrequentItemSet(Array("A", "C", "D"), 1),
          FrequentItemSet(Array("A", "C"), 1),
          FrequentItemSet(Array("A", "D"), 1),
          FrequentItemSet(Array("A"), 1),
          FrequentItemSet(Array("B", "C", "D"), 1),
          FrequentItemSet(Array("B", "C"), 1),
          FrequentItemSet(Array("B", "D"), 1),
          FrequentItemSet(Array("B"), 1),
          FrequentItemSet(Array("C", "D"), 1),
          FrequentItemSet(Array("C"), 1),
          FrequentItemSet(Array("D"), 1)
        )
      )
    )
  }

  test(
    "Mining of a 4 element database with minFrequency = 1 and minItemSetSize = 2 should return the 11 frequent itemsets"
  ) {
    val frequentItemSets = FPGrowth(Array(Array("A", "B", "C", "D")), 1).mine(minItemSetSize = 2)

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(
          FrequentItemSet(Array("A", "B", "C", "D"), 1),
          FrequentItemSet(Array("A", "B", "C"), 1),
          FrequentItemSet(Array("A", "B", "D"), 1),
          FrequentItemSet(Array("A", "B"), 1),
          FrequentItemSet(Array("A", "C", "D"), 1),
          FrequentItemSet(Array("A", "C"), 1),
          FrequentItemSet(Array("A", "D"), 1),
          FrequentItemSet(Array("B", "C", "D"), 1),
          FrequentItemSet(Array("B", "C"), 1),
          FrequentItemSet(Array("B", "D"), 1),
          FrequentItemSet(Array("C", "D"), 1)
        )
      )
    )
  }

  test(
    "Mining of a 4 element database with minFrequency = 1 and minItemSetSize = 3 should return the 4 frequent itemsets"
  ) {
    val frequentItemSets = FPGrowth(Array(Array("A", "B", "C", "D")), 1).mine(minItemSetSize = 3)

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(
          FrequentItemSet(Array("A", "B", "C", "D"), 1),
          FrequentItemSet(Array("A", "B", "C"), 1),
          FrequentItemSet(Array("A", "B", "D"), 1),
          FrequentItemSet(Array("A", "C", "D"), 1),
          FrequentItemSet(Array("B", "C", "D"), 1)
        )
      )
    )
  }

  test(
    "Mining of a 4 element database with minFrequency = 1 and minItemSetSize = 4 should return the 1 frequent itemsets"
  ) {
    val frequentItemSets = FPGrowth(Array(Array("A", "B", "C", "D")), 1).mine(minItemSetSize = 4)

    assert(ordered(frequentItemSets.items) === ordered(List(FrequentItemSet(Array("A", "B", "C", "D"), 1))))
  }

  test(
    "Mining of a 4 element database with minFrequency = 1 and maxItemSetSize = 3 should return the 14 frequent itemsets"
  ) {
    val frequentItemSets = FPGrowth(Array(Array("A", "B", "C", "D")), 1).mine(maxItemSetSize = 3)

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(
          FrequentItemSet(Array("A", "B", "C"), 1),
          FrequentItemSet(Array("A", "B", "D"), 1),
          FrequentItemSet(Array("A", "B"), 1),
          FrequentItemSet(Array("A", "C", "D"), 1),
          FrequentItemSet(Array("A", "C"), 1),
          FrequentItemSet(Array("A", "D"), 1),
          FrequentItemSet(Array("A"), 1),
          FrequentItemSet(Array("B", "C", "D"), 1),
          FrequentItemSet(Array("B", "C"), 1),
          FrequentItemSet(Array("B", "D"), 1),
          FrequentItemSet(Array("B"), 1),
          FrequentItemSet(Array("C", "D"), 1),
          FrequentItemSet(Array("C"), 1),
          FrequentItemSet(Array("D"), 1)
        )
      )
    )
  }

  test(
    "Mining of a 4 element database with minFrequency = 1 and maxItemSetSize = 2 should return the 10 frequent itemsets"
  ) {
    val frequentItemSets = FPGrowth(Array(Array("A", "B", "C", "D")), 1).mine(maxItemSetSize = 2)

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(
          FrequentItemSet(Array("A", "B"), 1),
          FrequentItemSet(Array("A", "C"), 1),
          FrequentItemSet(Array("A", "D"), 1),
          FrequentItemSet(Array("A"), 1),
          FrequentItemSet(Array("B", "C"), 1),
          FrequentItemSet(Array("B", "D"), 1),
          FrequentItemSet(Array("B"), 1),
          FrequentItemSet(Array("C", "D"), 1),
          FrequentItemSet(Array("C"), 1),
          FrequentItemSet(Array("D"), 1)
        )
      )
    )
  }

  test(
    "Mining of a 4 element database with minFrequency = 1 and maxItemSetSize = 1 should return the 4 frequent itemsets"
  ) {
    val frequentItemSets = FPGrowth(Array(Array("A", "B", "C", "D")), 1).mine(maxItemSetSize = 1)

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(
          FrequentItemSet(Array("A"), 1),
          FrequentItemSet(Array("B"), 1),
          FrequentItemSet(Array("C"), 1),
          FrequentItemSet(Array("D"), 1)
        )
      )
    )
  }

  test(
    "Mining of a 4 element database with minFrequency = 1 and minItemSetSize = 2 and maxItemSetSize = 3 should return the 10 frequent itemsets"
  ) {
    val frequentItemSets = FPGrowth(Array(Array("A", "B", "C", "D")), 1).mine(minItemSetSize = 2, maxItemSetSize = 3)

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(
          FrequentItemSet(Array("A", "B", "C"), 1),
          FrequentItemSet(Array("A", "B", "D"), 1),
          FrequentItemSet(Array("A", "B"), 1),
          FrequentItemSet(Array("A", "C", "D"), 1),
          FrequentItemSet(Array("A", "C"), 1),
          FrequentItemSet(Array("A", "D"), 1),
          FrequentItemSet(Array("B", "C", "D"), 1),
          FrequentItemSet(Array("B", "C"), 1),
          FrequentItemSet(Array("B", "D"), 1),
          FrequentItemSet(Array("C", "D"), 1)
        )
      )
    )
  }

  test(
    "Mining of a 4 element database with minFrequency = 1 and minItemSetSize = 2 and maxItemSetSize = 2 should return the 6 frequent itemsets"
  ) {
    val frequentItemSets = FPGrowth(Array(Array("A", "B", "C", "D")), 1).mine(minItemSetSize = 2, maxItemSetSize = 2)

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(
          FrequentItemSet(Array("A", "B"), 1),
          FrequentItemSet(Array("A", "C"), 1),
          FrequentItemSet(Array("A", "D"), 1),
          FrequentItemSet(Array("B", "C"), 1),
          FrequentItemSet(Array("B", "D"), 1),
          FrequentItemSet(Array("C", "D"), 1)
        )
      )
    )
  }

  test("Mining of a 24 element database with minFrequency = 1 should return 16777215 frequent itemsets") {
    val frequentItemSets = FPGrowth(
      Array(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)),
      1
    ).mineTo(1, 1, 0, 16777215, false, None, CountingAccumulator())

    assert(frequentItemSets.size === 16777215)
  }

  test("Mining {{A, B}, {C, D}}") {
    val frequentItemSets = FPGrowth(Array(Array("A", "B"), Array("C", "D")), 1).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(
          FrequentItemSet(Array("A", "B"), 1),
          FrequentItemSet(Array("A"), 1),
          FrequentItemSet(Array("B"), 1),
          FrequentItemSet(Array("C", "D"), 1),
          FrequentItemSet(Array("C"), 1),
          FrequentItemSet(Array("D"), 1)
        )
      )
    )
  }

  test("Mining {{A, B}}") {
    val frequentItemSets = FPGrowth(Array(Array("A", "B"), Array("A"), Array("B")), 2).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(List(FrequentItemSet(Array("A"), 2), FrequentItemSet(Array("B"), 2)))
    )
  }

  test("Mining of T10I4D100K database with minFrequency = 100") {
    val itemset = readItemSetFile("data/input/T10I4D100K.dat.gz")
    val expectedFrequentItemsets = readFrequentItemSetFile("data/output/T10I4D100K_fpg_mf-100.dat.gz")
    val frequentItemSets = FPGrowth(itemset, 100).mine()

    assert(ordered(frequentItemSets.items) === ordered(expectedFrequentItemsets))
  }

  test("Mining of T10I4D100K database") {
    forAll(List(5000, 1000, 500, 100)) { mf =>
      val itemset = readItemSetFile("data/input/T10I4D100K.dat.gz")
      val expectedFrequentItemsets = readFrequentItemSetFile(s"data/output/T10I4D100K_fpg_mf-$mf.dat.gz")
      val frequentItemSets = FPGrowth(itemset, mf).mine()

      assert(ordered(frequentItemSets.items) === ordered(expectedFrequentItemsets))
    }
  }

  test("Mining of T40I10D100K database") {
    forAll(List(5000, 1000, 500)) { mf =>
      val itemset = readItemSetFile("data/input/T40I10D100K.dat.gz")
      val expectedFrequentItemsets = readFrequentItemSetFile(s"data/output/T40I10D100K_fpg_mf-$mf.dat.gz")
      val frequentItemSets = FPGrowth(itemset, mf).mine()

      assert(ordered(frequentItemSets.items) === ordered(expectedFrequentItemsets))
    }
  }

}
