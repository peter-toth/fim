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

import org.openjdk.jmh.annotations._
import ptoth.fim.{FrequentItemSetAccumulator, FrequentItemSetUtils}

@State(Scope.Benchmark)
class FPGrowthT40I10D100KBenchmark {

  var itemset: Array[Array[String]] = _

  @Setup
  def setup(): Unit = itemset = FrequentItemSetUtils.readItemSetFile("data/input/T40I10D100K.dat")

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @Fork(3)
  @Warmup(iterations = 5)
  @Measurement(iterations = 5)
  def measureFPGrowthT40I10D100K(): FrequentItemSetAccumulator[String] =
    FPGrowthBenchmark.measureFPGrowth(itemset, 1000)

}
