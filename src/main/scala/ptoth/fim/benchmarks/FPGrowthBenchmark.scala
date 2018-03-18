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

package ptoth.fim.benchmarks

import ptoth.fim.fpgrowth.FPGrowth
import ptoth.fim.{ FrequentItemSet, FrequentItemSetAccumulator }

object FPGrowthBenchmark {

  def measureFPGrowth(itemsets: Array[Array[Int]], minFrequency: Int): FrequentItemSetAccumulator[Int] = {
    val dummyAccumulator: FrequentItemSetAccumulator[Int] = new FrequentItemSetAccumulator[Int] {

      override def add(frequentItemSet: FrequentItemSet[Int]): this.type = this

      override def size: Int = 0

    }

    FPGrowth(itemsets, minFrequency, accumulator = dummyAccumulator)

    dummyAccumulator
  }

}
