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

import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import scala.collection.mutable.ListBuffer
import scala.io.Source

object FrequentItemSetUtils {

  def ordered[ItemType: Ordering](
      frequentItemSets: Seq[FrequentItemSet[ItemType]]
  )(implicit ord: Ordering[FrequentItemSet[ItemType]]): Seq[FrequentItemSet[ItemType]] =
    frequentItemSets.map(_.toOrdered).sorted

  def readItemSetFile(file: String, compressed: Boolean = true): Array[Array[String]] = {
    val buffer = ListBuffer.empty[Array[String]]
    val source =
      if (compressed) Source.fromInputStream(new GZIPInputStream(new FileInputStream(file))) else Source.fromFile(file)
    for (line <- source.getLines) {
      val items = line.split(' ')
      buffer += items
    }

    buffer.toArray
  }

  def readFrequentItemSetFile(file: String, compressed: Boolean = true): Array[FrequentItemSet[String]] = {
    val buffer = ListBuffer.empty[FrequentItemSet[String]]
    val source =
      if (compressed) Source.fromInputStream(new GZIPInputStream(new FileInputStream(file))) else Source.fromFile(file)
    for (line <- source.getLines) {
      val itemsAndFrequency  = line.split(' ')
      val (items, frequency) = itemsAndFrequency.splitAt(itemsAndFrequency.length - 1)
      buffer += FrequentItemSet.empty[String].addItems(items, frequency(0).drop(1).dropRight(1).toInt)
    }

    buffer.toArray
  }

}
