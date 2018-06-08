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
import ptoth.fim.{ FrequentItemSet }
import ptoth.fim.FrequentItemSetUtils._
import org.scalatest.Inspectors._

class FPMaxTest extends FunSuite {

  test("Mining of {}") {
    val frequentItemSets = FPMax(Array.empty[Array[String]], 1).mine()

    assert(frequentItemSets.size === 0)
  }

  test("Mining of {{A}}") {
    val frequentItemSets = FPMax(Array(Array("A")), 1).mine()

    assert(ordered(frequentItemSets.items) === ordered(List(FrequentItemSet(Array("A"), 1))))
  }

  test("Mining of {{A, B}}") {
    val frequentItemSets = FPMax(Array(Array("A", "B")), 1).mine()

    assert(ordered(frequentItemSets.items) === ordered(List(FrequentItemSet(Array("A", "B"), 1))))
  }

  test("Mining of {{A, B, C}}") {
    val frequentItemSets = FPMax(Array(Array("A", "B", "C")), 1).mine()

    assert(ordered(frequentItemSets.items) === ordered(List(FrequentItemSet(Array("A", "B", "C"), 1))))
  }

  test("Mining of a {{A}, {B}}") {
    val frequentItemSets = FPMax(Array(Array("A"), Array("B")), 1).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(List(FrequentItemSet(Array("A"), 1), FrequentItemSet(Array("B"), 1)))
    )
  }

  test("Mining of a {{A, B}, {A}}") {
    val frequentItemSets = FPMax(Array(Array("A", "B"), Array("A")), 1).mine()

    assert(ordered(frequentItemSets.items) === ordered(List(FrequentItemSet(Array("A", "B"), 1))))
  }

  test("Mining of a {{A, B}, {A, B}}") {
    val frequentItemSets = FPMax(Array(Array("A", "B"), Array("A", "B")), 1).mine()

    assert(ordered(frequentItemSets.items) === ordered(List(FrequentItemSet(Array("A", "B"), 2))))
  }

  test("Mining of a {{A, B}, {C}}") {
    val frequentItemSets = FPMax(Array(Array("A", "B"), Array("C")), 1).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(FrequentItemSet(Array("A", "B"), 1), FrequentItemSet(Array("C"), 1))
      )
    )
  }

  test("Mining of a {{A, B}, {A, B}, {A}, {C}}") {
    val frequentItemSets = FPMax(Array(Array("A", "B"), Array("A", "B"), Array("A"), Array("C")), 1).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(FrequentItemSet(Array("A", "B"), 2), FrequentItemSet(Array("C"), 1))
      )
    )
  }

  test("Mining of a {{A, B}, {A, B}, {A, C}}") {
    val frequentItemSets = FPMax(Array(Array("A", "B"), Array("A", "B"), Array("A", "C")), 1).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(FrequentItemSet(Array("A", "B"), 2), FrequentItemSet(Array("A", "C"), 1))
      )
    )
  }

  test("Mining of a {{A, B, C}, {A, B}, {A}, {A}, {C}, {C}") {
    val frequentItemSets =
      FPMax(Array(Array("A", "B", "C"), Array("A", "B"), Array("A"), Array("A"), Array("C"), Array("C")), 1).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(FrequentItemSet(Array("A", "B", "C"), 1))
      )
    )
  }

  test("Mining of a {{A, B, C}, {A, B}, {A, D}, {A, D}, {C, E}, {C, E}") {
    val frequentItemSets =
      FPMax(Array(Array("A", "B", "C"),
                  Array("A", "B"),
                  Array("A", "D"),
                  Array("A", "D"),
                  Array("C", "E"),
                  Array("C", "E")),
            1).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(FrequentItemSet(Array("A", "B", "C"), 1),
             FrequentItemSet(Array("A", "D"), 2),
             FrequentItemSet(Array("C", "E"), 2))
      )
    )
  }

  test("Mining of T10I4D100K database with minFrequency = 100") {
    val itemset                  = readItemSetFile("data/input/T10I4D100K.dat.gz")
    val expectedFrequentItemsets = readFrequentItemSetFile("data/output/T10I4D100K_fpm_mf-100.dat.gz")
    val frequentItemSets         = FPMax(itemset, 100).mine()

    assert(ordered(frequentItemSets.items) === ordered(expectedFrequentItemsets))
  }

  test("Mining of T10I4D100K database") {
    forAll(List(5000, 1000, 500, 100)) { mf =>
      val itemset                  = readItemSetFile("data/input/T10I4D100K.dat.gz")
      val expectedFrequentItemsets = readFrequentItemSetFile(s"data/output/T10I4D100K_fpm_mf-$mf.dat.gz")
      val frequentItemSets         = FPMax(itemset, mf).mine()

      assert(ordered(frequentItemSets.items) === ordered(expectedFrequentItemsets))
    }
  }

  test("Mining of T40I10D100K database") {
    forAll(List(5000, 1000, 500)) { mf =>
      val itemset                  = readItemSetFile("data/input/T40I10D100K.dat.gz")
      val expectedFrequentItemsets = readFrequentItemSetFile(s"data/output/T40I10D100K_fpm_mf-$mf.dat.gz")
      val frequentItemSets         = FPMax(itemset, mf).mine()

      assert(ordered(frequentItemSets.items) === ordered(expectedFrequentItemsets))
    }
  }

}
