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
import org.scalatest.Inspectors._
import ptoth.fim.FrequentItemSet
import ptoth.fim.FrequentItemSetUtils._

class FPCloseTest extends FunSuite {

  test("Mining of {}") {
    val frequentItemSets = FPClose(Array.empty[Array[String]], 1).mine()

    assert(frequentItemSets.size === 0)
  }

  test("Mining of {{A}}") {
    val frequentItemSets = FPClose(Array(Array("A")), 1).mine()

    assert(ordered(frequentItemSets.items) === ordered(List(FrequentItemSet(Array("A"), 1))))
  }

  test("Mining of {{A, B}}") {
    val frequentItemSets = FPClose(Array(Array("A", "B")), 1).mine()

    assert(ordered(frequentItemSets.items) === ordered(List(FrequentItemSet(Array("A", "B"), 1))))
  }

  test("Mining of {{A, B, C}}") {
    val frequentItemSets = FPClose(Array(Array("A", "B", "C")), 1).mine()

    assert(ordered(frequentItemSets.items) === ordered(List(FrequentItemSet(Array("A", "B", "C"), 1))))
  }

  test("Mining of a {{A}, {B}}") {
    val frequentItemSets = FPClose(Array(Array("A"), Array("B")), 1).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(List(FrequentItemSet(Array("A"), 1), FrequentItemSet(Array("B"), 1)))
    )
  }

  test("Mining of a {{A, B}, {A}}") {
    val frequentItemSets = FPClose(Array(Array("A", "B"), Array("A")), 1).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(FrequentItemSet(Array("A", "B"), 1), FrequentItemSet(Array("A"), 2))
      )
    )
  }

  test("Mining of a {{A, B}, {A, B}}") {
    val frequentItemSets = FPClose(Array(Array("A", "B"), Array("A", "B")), 1).mine()

    assert(ordered(frequentItemSets.items) === ordered(List(FrequentItemSet(Array("A", "B"), 2))))
  }

  test("Mining of a {{A, B}, {C}}") {
    val frequentItemSets = FPClose(Array(Array("A", "B"), Array("C")), 1).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(FrequentItemSet(Array("A", "B"), 1), FrequentItemSet(Array("C"), 1))
      )
    )
  }

  test("Mining of a {{A, B}, {A, B}, {A}, {C}}") {
    val frequentItemSets = FPClose(Array(Array("A", "B"), Array("A", "B"), Array("A"), Array("C")), 1).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(FrequentItemSet(Array("A", "B"), 2), FrequentItemSet(Array("A"), 3), FrequentItemSet(Array("C"), 1))
      )
    )
  }

  test("Mining of a {{A, B}, {A, B}, {A, C}}") {
    val frequentItemSets = FPClose(Array(Array("A", "B"), Array("A", "B"), Array("A", "C")), 1).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(FrequentItemSet(Array("A", "B"), 2), FrequentItemSet(Array("A", "C"), 1), FrequentItemSet(Array("A"), 3))
      )
    )
  }

  test("Mining of a {{A, B, C}, {A, B}, {A}, {A}, {C}, {C}") {
    val frequentItemSets =
      FPClose(Array(Array("A", "B", "C"), Array("A", "B"), Array("A"), Array("A"), Array("C"), Array("C")), 1).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(FrequentItemSet(Array("A", "B", "C"), 1),
             FrequentItemSet(Array("A", "B"), 2),
             FrequentItemSet(Array("A"), 4),
             FrequentItemSet(Array("C"), 3))
      )
    )
  }

  test("Mining of a {{A, C, D}, {B, C, D}, {A}, {A}, {B}, {B}}") {
    val frequentItemSets =
      FPClose(
        Array(
          Array("A", "C", "D"),
          Array("B", "C", "D"),
          Array("A"),
          Array("A"),
          Array("B"),
          Array("B")
        ),
        1
      ).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(
          FrequentItemSet(Array("A", "C", "D"), 1),
          FrequentItemSet(Array("B", "C", "D"), 1),
          FrequentItemSet(Array("C", "D"), 2),
          FrequentItemSet(Array("A"), 3),
          FrequentItemSet(Array("B"), 3)
        )
      )
    )
  }

  test("Mining of a {{A, C, D}:2, {B, C, D}, {A}, {A}, {B}, {B}}") {
    val frequentItemSets =
      FPClose(
        Array(
          Array("A", "C", "D"),
          Array("A", "C", "D"),
          Array("B", "C", "D"),
          Array("A"),
          Array("A"),
          Array("B"),
          Array("B")
        ),
        1
      ).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(
          FrequentItemSet(Array("A", "C", "D"), 2),
          FrequentItemSet(Array("B", "C", "D"), 1),
          FrequentItemSet(Array("C", "D"), 3),
          FrequentItemSet(Array("A"), 4),
          FrequentItemSet(Array("B"), 3)
        )
      )
    )
  }

  test("Mining of a {{A, C}:2, {B, C}, {A}:2, {B}:2}") {
    val frequentItemSets =
      FPClose(
        Array(
          Array("C", "A"),
          Array("C", "A"),
          Array("C", "B"),
          Array("A"),
          Array("A"),
          Array("A"),
          Array("B"),
          Array("B"),
          Array("B")
        ),
        1
      ).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(
          FrequentItemSet(Array("A", "C"), 2),
          FrequentItemSet(Array("B", "C"), 1),
          FrequentItemSet(Array("A"), 5),
          FrequentItemSet(Array("B"), 4),
          FrequentItemSet(Array("C"), 3)
        )
      )
    )
  }

  test("Mining of a {{A, C, D}:2, {B, C, D}, {D, A}:3, {D, B}:3, {A}:7, {B}:7, {C}:7}") {
    val frequentItemSets =
      FPClose(
        Array(
          Array("D", "C", "A"),
          Array("D", "C", "A"),
          Array("D", "C", "B"),
          Array("D", "A"),
          Array("D", "A"),
          Array("D", "A"),
          Array("D", "B"),
          Array("D", "B"),
          Array("D", "B"),
          Array("A"),
          Array("A"),
          Array("A"),
          Array("A"),
          Array("A"),
          Array("A"),
          Array("A"),
          Array("B"),
          Array("B"),
          Array("B"),
          Array("B"),
          Array("B"),
          Array("B"),
          Array("B"),
          Array("C"),
          Array("C"),
          Array("C"),
          Array("C"),
          Array("C"),
          Array("C"),
          Array("C")
        ),
        1
      ).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(
          FrequentItemSet(Array("A", "C", "D"), 2),
          FrequentItemSet(Array("B", "C", "D"), 1),
          FrequentItemSet(Array("A", "D"), 5),
          FrequentItemSet(Array("B", "D"), 4),
          FrequentItemSet(Array("C", "D"), 3),
          FrequentItemSet(Array("A"), 12),
          FrequentItemSet(Array("B"), 11),
          FrequentItemSet(Array("C"), 10),
          FrequentItemSet(Array("D"), 9)
        )
      )
    )
  }

  test("Mining of a {{A, C, D}, {B, C, D}, {C, D}, {A}:4, {B}:3") {
    val frequentItemSets =
      FPClose(
        Array(
          Array("A", "C", "D"),
          Array("B", "C", "D"),
          Array("C", "D"),
          Array("A"),
          Array("A"),
          Array("A"),
          Array("A"),
          Array("B"),
          Array("B"),
          Array("B")
        ),
        1
      ).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(
          FrequentItemSet(Array("A", "C", "D"), 1),
          FrequentItemSet(Array("B", "C", "D"), 1),
          FrequentItemSet(Array("C", "D"), 3),
          FrequentItemSet(Array("A"), 5),
          FrequentItemSet(Array("B"), 4)
        )
      )
    )
  }

  test("Mining of a {{A, B, C}, {A, B}, {A, D}, {A, D}, {C, E}, {C, E}") {
    val frequentItemSets =
      FPClose(Array(Array("A", "B", "C"),
                    Array("A", "B"),
                    Array("A", "D"),
                    Array("A", "D"),
                    Array("C", "E"),
                    Array("C", "E")),
              1).mine()

    assert(
      ordered(frequentItemSets.items) === ordered(
        List(
          FrequentItemSet(Array("A", "B", "C"), 1),
          FrequentItemSet(Array("A", "B"), 2),
          FrequentItemSet(Array("A", "D"), 2),
          FrequentItemSet(Array("C", "E"), 2),
          FrequentItemSet(Array("A"), 4),
          FrequentItemSet(Array("C"), 3)
        )
      )
    )
  }

  test("Mining of T10I4D100K database") {
    forAll(List(5000, 1000, 500, 100)) { mf =>
      val itemset                  = readItemSetFile("data/input/T10I4D100K.dat.gz")
      val expectedFrequentItemsets = readFrequentItemSetFile(s"data/output/T10I4D100K_fpc_mf-$mf.dat.gz")
      val frequentItemSets         = FPClose(itemset, mf).mine()

      assert(ordered(frequentItemSets.items) === ordered(expectedFrequentItemsets))
    }
  }

  test("Mining of T40I10D100K database") {
    forAll(List(5000, 1000, 500)) { mf =>
      val itemset                  = readItemSetFile("data/input/T40I10D100K.dat.gz")
      val expectedFrequentItemsets = readFrequentItemSetFile(s"data/output/T40I10D100K_fpc_mf-$mf.dat.gz")
      val frequentItemSets         = FPClose(itemset, mf).mine()

      assert(ordered(frequentItemSets.items) === ordered(expectedFrequentItemsets))
    }
  }

//  test("Mining of T10I4D100K database with minFrequency = 100") {
//    val itemset                  = readItemSetFile("data/input/T10I4D100K.dat.gz")
//    val expectedFrequentItemsets = readFrequentItemSetFile("data/output/T10I4D100K_fpc_mf-1000.dat.gz")
//    val frequentItemSets         = FPClose(itemset, 1000).mine()
//
//    assert(ordered(frequentItemSets.items) === ordered(expectedFrequentItemsets))
//  }

}
