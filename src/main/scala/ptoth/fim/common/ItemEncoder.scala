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

package ptoth.fim.common

trait ItemEncoder[ItemType] {

  def minFrequency: Int
  def itemFrequencies: Array[(ItemType, Int)]
  def encodeItem(item: ItemType): Option[Int]
  def encodeItems(itemset: Array[ItemType]): Array[Int] = itemset.flatMap(encodeItem).distinct.sorted

  def decodeItem(itemId: Int): Option[ItemType] =
    if (itemId >= 0 && itemId < itemFrequencies.length) Some(itemFrequencies(itemId)._1) else None

}

case class MapEncoder[ItemType](allItemFrequencies: collection.Map[ItemType, Int], minFrequency: Int)
    extends ItemEncoder[ItemType] {

  override val itemFrequencies: Array[(ItemType, Int)] =
    allItemFrequencies.filter(_._2 >= minFrequency).toArray.sortBy(-_._2)

  private val itemToItemId = itemFrequencies.map(_._1).zipWithIndex.toMap

  override def encodeItem(item: ItemType): Option[Int] = itemToItemId.get(item)

}

case class ContinuousArrayEncoder(allItemFrequencies: Array[Int], minFrequency: Int) extends ItemEncoder[Int] {

  override val itemFrequencies: Array[(Int, Int)] =
    allItemFrequencies.zipWithIndex.filter(_._1 >= minFrequency).sortBy(-_._1).map(_.swap)

  private val itemToItemId = {
    val a = Array.fill(allItemFrequencies.length)(-1)
    Iterator.range(0, itemFrequencies.length).foreach(i => a(itemFrequencies(i)._1) = i)
    a
  }

  override def encodeItem(item: Int): Option[Int] = if (itemToItemId(item) != -1) Some(itemToItemId(item)) else None

}
