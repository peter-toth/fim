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

package ptoth.fim.FPMax

import org.scalatest.FunSuite
import ptoth.fim.fpgrowth.FPMax
import ptoth.fim.{ CountingAccumulator, FrequentItemSet, FrequentItemSetUtils }

class FPMaxTest extends FunSuite {

  test("Mining of {}") {
    val frequentItemSets = FPMax(Array.empty[Array[String]], 1)

    assert(frequentItemSets.size === 0)
  }

  test("Mining of {{A}}") {
    val frequentItemSets = FPMax(Array(Array("A")), 1)

    assert(frequentItemSets.set === Set(FrequentItemSet(Array("A"), 1)))
  }

  test("Mining of {{A, B}}") {
    val frequentItemSets = FPMax(Array(Array("A", "B")), 1)

    assert(
      frequentItemSets.set === Set(FrequentItemSet(Array("A", "B"), 1))
    )
  }

  test("Mining of {{A, B, C}}") {
    val frequentItemSets = FPMax(Array(Array("A", "B", "C")), 1)

    assert(
      frequentItemSets.set === Set(FrequentItemSet(Array("A", "B", "C"), 1))
    )
  }

  test("Mining of a {{A}, {B}}") {
    val frequentItemSets = FPMax(Array(Array("A"), Array("B")), 1)

    assert(
      frequentItemSets.set === Set(FrequentItemSet(Array("A"), 1), FrequentItemSet(Array("B"), 1))
    )
  }

  test("Mining of a {{A, B}, {A}}") {
    val frequentItemSets = FPMax(Array(Array("A", "B"), Array("A")), 1)

    assert(
      frequentItemSets.set === Set(FrequentItemSet(Array("A", "B"), 1))
    )
  }

  test("Mining of a {{A, B}, {A, B}}") {
    val frequentItemSets = FPMax(Array(Array("A", "B"), Array("A", "B")), 1)

    assert(
      frequentItemSets.set === Set(FrequentItemSet(Array("A", "B"), 2))
    )
  }

  test("Mining of a {{A, B}, {C}}") {
    val frequentItemSets = FPMax(Array(Array("A", "B"), Array("C")), 1)

    assert(
      frequentItemSets.set === Set(FrequentItemSet(Array("A", "B"), 1), FrequentItemSet(Array("C"), 1))
    )
  }

  test("Mining of T10I4D100K database with minFrequency = 500 should return 10 frequent itemsets") {
    val itemset          = FrequentItemSetUtils.readItemSetFile("data/T10I4D100K.dat")
    val frequentItemSets = FPMax(itemset, 500)

    assert(frequentItemSets.size == 585)
  }

  /*test("Mining of a one element database with minFrequency = 2 should return nothing") {
    val frequentItemSets = FPMax(Array(Array("A")), 2)

    assert(frequentItemSets.size == 0)
  }


  test("Mining of a 3 element database with minFrequency = 1 should return the 6 frequent itemsets") {
    val frequentItemSets = FPMax(Array(Array("A", "B", "C")), 1)

    assert(
      frequentItemSets.set ==
        Set(
          FrequentItemSet(Array("A"), 1),
          FrequentItemSet(Array("A", "B"), 1),
          FrequentItemSet(Array("A", "B", "C"), 1),
          FrequentItemSet(Array("A", "C"), 1),
          FrequentItemSet(Array("B"), 1),
          FrequentItemSet(Array("B", "C"), 1),
          FrequentItemSet(Array("C"), 1)
        )
    )
  }

  test("Mining of a 4 element database with minFrequency = 1 should return the 15 frequent itemsets") {
    val frequentItemSets = FPMax(Array(Array("A", "B", "C", "D")), 1)

    assert(
      frequentItemSets.set ==
        Set(
          FrequentItemSet(Array("A"), 1),
          FrequentItemSet(Array("A", "B"), 1),
          FrequentItemSet(Array("A", "B", "C"), 1),
          FrequentItemSet(Array("A", "B", "C", "D"), 1),
          FrequentItemSet(Array("A", "B", "D"), 1),
          FrequentItemSet(Array("A", "C"), 1),
          FrequentItemSet(Array("A", "C", "D"), 1),
          FrequentItemSet(Array("A", "D"), 1),
          FrequentItemSet(Array("B"), 1),
          FrequentItemSet(Array("B", "C"), 1),
          FrequentItemSet(Array("B", "C", "D"), 1),
          FrequentItemSet(Array("B", "D"), 1),
          FrequentItemSet(Array("C"), 1),
          FrequentItemSet(Array("C", "D"), 1),
          FrequentItemSet(Array("D"), 1)
        )
    )
  }

  test(
    "Mining of a 4 element database with minFrequency = 1 and minItemSetSize = 2 should return the 11 frequent itemsets"
  ) {
    val frequentItemSets = FPMax(Array(Array("A", "B", "C", "D")), 1, minItemSetSize = 2)

    assert(
      frequentItemSets.set ==
        Set(
          FrequentItemSet(Array("A", "B"), 1),
          FrequentItemSet(Array("A", "B", "C"), 1),
          FrequentItemSet(Array("A", "B", "C", "D"), 1),
          FrequentItemSet(Array("A", "B", "D"), 1),
          FrequentItemSet(Array("A", "C"), 1),
          FrequentItemSet(Array("A", "C", "D"), 1),
          FrequentItemSet(Array("A", "D"), 1),
          FrequentItemSet(Array("B", "C"), 1),
          FrequentItemSet(Array("B", "C", "D"), 1),
          FrequentItemSet(Array("B", "D"), 1),
          FrequentItemSet(Array("C", "D"), 1)
        )
    )
  }

  test(
    "Mining of a 4 element database with minFrequency = 1 and minItemSetSize = 3 should return the 4 frequent itemsets"
  ) {
    val frequentItemSets = FPMax(Array(Array("A", "B", "C", "D")), 1, minItemSetSize = 3)

    assert(
      frequentItemSets.set ==
        Set(
          FrequentItemSet(Array("A", "B", "C"), 1),
          FrequentItemSet(Array("A", "B", "C", "D"), 1),
          FrequentItemSet(Array("A", "B", "D"), 1),
          FrequentItemSet(Array("A", "C", "D"), 1),
          FrequentItemSet(Array("B", "C", "D"), 1)
        )
    )
  }

  test(
    "Mining of a 4 element database with minFrequency = 1 and minItemSetSize = 4 should return the 1 frequent itemsets"
  ) {
    val frequentItemSets = FPMax(Array(Array("A", "B", "C", "D")), 1, minItemSetSize = 4)

    assert(
      frequentItemSets.set ==
        Set(
          FrequentItemSet(Array("A", "B", "C", "D"), 1)
        )
    )
  }

  test(
    "Mining of a 4 element database with minFrequency = 1 and maxItemSetSize = 3 should return the 14 frequent itemsets"
  ) {
    val frequentItemSets = FPMax(Array(Array("A", "B", "C", "D")), 1, maxItemSetSize = 3)

    assert(
      frequentItemSets.set ==
        Set(
          FrequentItemSet(Array("A"), 1),
          FrequentItemSet(Array("A", "B"), 1),
          FrequentItemSet(Array("A", "B", "C"), 1),
          FrequentItemSet(Array("A", "B", "D"), 1),
          FrequentItemSet(Array("A", "C"), 1),
          FrequentItemSet(Array("A", "C", "D"), 1),
          FrequentItemSet(Array("A", "D"), 1),
          FrequentItemSet(Array("B"), 1),
          FrequentItemSet(Array("B", "C"), 1),
          FrequentItemSet(Array("B", "C", "D"), 1),
          FrequentItemSet(Array("B", "D"), 1),
          FrequentItemSet(Array("C"), 1),
          FrequentItemSet(Array("C", "D"), 1),
          FrequentItemSet(Array("D"), 1)
        )
    )
  }

  test(
    "Mining of a 4 element database with minFrequency = 1 and maxItemSetSize = 2 should return the 10 frequent itemsets"
  ) {
    val frequentItemSets = FPMax(Array(Array("A", "B", "C", "D")), 1, maxItemSetSize = 2)

    assert(
      frequentItemSets.set ==
        Set(
          FrequentItemSet(Array("A"), 1),
          FrequentItemSet(Array("A", "B"), 1),
          FrequentItemSet(Array("A", "C"), 1),
          FrequentItemSet(Array("A", "D"), 1),
          FrequentItemSet(Array("B"), 1),
          FrequentItemSet(Array("B", "C"), 1),
          FrequentItemSet(Array("B", "D"), 1),
          FrequentItemSet(Array("C"), 1),
          FrequentItemSet(Array("C", "D"), 1),
          FrequentItemSet(Array("D"), 1)
        )
    )
  }

  test(
    "Mining of a 4 element database with minFrequency = 1 and maxItemSetSize = 1 should return the 4 frequent itemsets"
  ) {
    val frequentItemSets = FPMax(Array(Array("A", "B", "C", "D")), 1, maxItemSetSize = 1)

    assert(
      frequentItemSets.set ==
        Set(
          FrequentItemSet(Array("A"), 1),
          FrequentItemSet(Array("B"), 1),
          FrequentItemSet(Array("C"), 1),
          FrequentItemSet(Array("D"), 1)
        )
    )
  }

  test(
    "Mining of a 4 element database with minFrequency = 1 and minItemSetSize = 2 and maxItemSetSize = 3 should return the 10 frequent itemsets"
  ) {
    val frequentItemSets = FPMax(Array(Array("A", "B", "C", "D")), 1, minItemSetSize = 2, maxItemSetSize = 3)

    assert(
      frequentItemSets.set ==
        Set(
          FrequentItemSet(Array("A", "B"), 1),
          FrequentItemSet(Array("A", "B", "C"), 1),
          FrequentItemSet(Array("A", "B", "D"), 1),
          FrequentItemSet(Array("A", "C"), 1),
          FrequentItemSet(Array("A", "C", "D"), 1),
          FrequentItemSet(Array("A", "D"), 1),
          FrequentItemSet(Array("B", "C"), 1),
          FrequentItemSet(Array("B", "C", "D"), 1),
          FrequentItemSet(Array("B", "D"), 1),
          FrequentItemSet(Array("C", "D"), 1)
        )
    )
  }

  test(
    "Mining of a 4 element database with minFrequency = 1 and minItemSetSize = 2 and maxItemSetSize = 2 should return the 6 frequent itemsets"
  ) {
    val frequentItemSets = FPMax(Array(Array("A", "B", "C", "D")), 1, minItemSetSize = 2, maxItemSetSize = 2)

    assert(
      frequentItemSets.set ==
        Set(
          FrequentItemSet(Array("A", "B"), 1),
          FrequentItemSet(Array("A", "C"), 1),
          FrequentItemSet(Array("A", "D"), 1),
          FrequentItemSet(Array("B", "C"), 1),
          FrequentItemSet(Array("B", "D"), 1),
          FrequentItemSet(Array("C", "D"), 1)
        )
    )
  }

  test("Mining of a 24 element database with minFrequency = 1 should return 16777215 frequent itemsets") {
    val frequentItemSets = FPMax(
      Array(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)),
      1,
      1,
      0,
      16777215,
      false,
      None,
      CountingAccumulator()
    )

    assert(frequentItemSets.size == 16777215)
  }


  test("Mining of T40I10D100K database with minFrequency = 1000 should return 65236 frequent itemsets") {
    val itemset          = FrequentItemSetUtils.readItemSetFile("data/T40I10D100K.dat")
    val frequentItemSets = FPMax(itemset, 1000)

    assert(frequentItemSets.size == 65236)
  }*/

}
