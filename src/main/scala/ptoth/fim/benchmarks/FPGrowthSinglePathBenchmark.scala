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
import ptoth.fim.FrequentItemSetAccumulator

@State(Scope.Benchmark)
class FPGrowthSinglePathBenchmark {

  private val itemset20 = Array(
    Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
  )
  private val itemset24 = Array(
    Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)
  )
  private val itemset28 = Array(
    Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
  )
  private val itemset30 = Array(
    Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)
  )

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @Fork(3)
  @Warmup(iterations = 5)
  @Measurement(iterations = 5)
  def measureFPGrowth20(): FrequentItemSetAccumulator[Int] = FPGrowthBenchmark.measureFPGrowth(itemset20, 1)

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @Fork(3)
  @Warmup(iterations = 5)
  @Measurement(iterations = 5)
  def measureFPGrowth24(): FrequentItemSetAccumulator[Int] = FPGrowthBenchmark.measureFPGrowth(itemset24, 1)

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @Fork(3)
  @Warmup(iterations = 5)
  @Measurement(iterations = 5)
  def measureFPGrowth28(): FrequentItemSetAccumulator[Int] = FPGrowthBenchmark.measureFPGrowth(itemset28, 1)

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @Fork(3)
  @Warmup(iterations = 5)
  @Measurement(iterations = 5)
  def measureFPGrowth30(): FrequentItemSetAccumulator[Int] = FPGrowthBenchmark.measureFPGrowth(itemset30, 1)

}
