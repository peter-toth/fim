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

import java.util.Objects

import scala.reflect.ClassTag

case class FrequentItemSet[ItemType: ClassTag](items: Array[ItemType], frequency: Int) {

  def size: Int = items.length

  def isEmpty: Boolean = size == 0

  def addItem(item: ItemType, frequency: Int): FrequentItemSet[ItemType] = {
    val items = new Array[ItemType](this.items.length + 1)
    items(0) = item
    this.items.copyToArray(items, 1)

    FrequentItemSet(items, frequency)
  }

  def addItems(newItems: Array[ItemType], frequency: Int): FrequentItemSet[ItemType] = {
    val items = new Array[ItemType](this.items.length + newItems.length)
    newItems.copyToArray(items, 0, newItems.length)
    this.items.copyToArray(items, newItems.length, this.items.length)

    FrequentItemSet(items, frequency)
  }

  override def toString: String = s"{${items.mkString(", ")}}: $frequency"

  override def hashCode(): Int = Objects.hash(Int.box(frequency), items.toSet)

  override def equals(obj: Any): Boolean =
    obj.isInstanceOf[FrequentItemSet[ItemType]] && obj
      .asInstanceOf[FrequentItemSet[ItemType]]
      .frequency == frequency && obj.asInstanceOf[FrequentItemSet[ItemType]].items.toSet == items.toSet

}

object FrequentItemSet {

  def empty[ItemType: ClassTag]: FrequentItemSet[ItemType] = FrequentItemSet(Array.empty[ItemType], 0)

}
