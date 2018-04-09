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

package ptoth.fim

import scala.collection.mutable.ListBuffer

trait FrequentItemSetAccumulator[ItemType] {

  def add(frequentItemSet: FrequentItemSet[ItemType]): this.type
  def size: Int

}

case class ListAccumulator[ItemType]() extends FrequentItemSetAccumulator[ItemType] {

  private val list: ListBuffer[FrequentItemSet[ItemType]] = ListBuffer.empty[FrequentItemSet[ItemType]]

  override def add(frequentItemSet: FrequentItemSet[ItemType]): this.type = {
    list += frequentItemSet

    this
  }

  override def size: Int = list.size

  def items: List[FrequentItemSet[ItemType]] = list.toList

}

case class CountingAccumulator[ItemType]() extends FrequentItemSetAccumulator[ItemType] {

  private var n: Int = 0

  override def add(frequentItemSet: FrequentItemSet[ItemType]): this.type = {
    n += 1

    this
  }

  override def size: Int = n

}
