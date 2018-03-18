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

import ptoth.fim.{ FrequentItemSet, FrequentItemSetAccumulator, SetAccumulator }

import scala.reflect.ClassTag

object FPGrowth {

  def apply[ItemType: ClassTag](
      itemSets: Array[Array[ItemType]],
      minFrequency: Int,
      minItemSetSize: Int = 1,
      maxItemSetSize: Int = 0,
      maxNItemSets: Int = 1000000,
      enableParallel: Boolean = true,
      baseItemSet: Option[FrequentItemSet[ItemType]] = None,
      accumulator: FrequentItemSetAccumulator[ItemType] = SetAccumulator[ItemType]()
  ): accumulator.type = {
    val fpTree = FPTree(itemSets, minFrequency)

    mine(fpTree,
         minFrequency,
         minItemSetSize,
         maxItemSetSize,
         maxNItemSets,
         enableParallel,
         baseItemSet.getOrElse(FrequentItemSet.empty),
         accumulator)
  }

  private def mine[ItemType: ClassTag](
      fpTree: FPTree[ItemType],
      minFrequency: Int,
      minItemSetSize: Int,
      maxItemSetSize: Int,
      maxNItemSets: Int,
      enableParallel: Boolean,
      baseItemSet: FrequentItemSet[ItemType],
      accumulator: FrequentItemSetAccumulator[ItemType]
  ): accumulator.type = {
    if (maxItemSetSize == 0 || baseItemSet.size < maxItemSetSize) {
      //val parallel = fpTree.nNodes > 20 && enableParallel

      var itemId = 0.max(minItemSetSize - baseItemSet.size - 1)
      while (itemId < fpTree.headers.size) {
        val header = fpTree.headers(itemId)

        val frequentItemSet = baseItemSet.add(header.item, header.frequency)
        if (frequentItemSet.size >= minItemSetSize && accumulator.size < maxNItemSets) {
          accumulator.add(frequentItemSet)
        }

        if (header.node.sibling == null) {
          var node   = header.node.parent
          var height = 0
          while (node != null) {
            node = node.parent;
            height += 1;
          }

          mineSinglePath(header.node,
                         height,
                         fpTree.headers,
                         header.frequency,
                         minItemSetSize,
                         maxItemSetSize,
                         maxNItemSets,
                         enableParallel,
                         frequentItemSet,
                         accumulator)
        } else {
          val conditionalTree = FPTree(fpTree, itemId, minFrequency)

          mine(conditionalTree,
               minFrequency,
               minItemSetSize,
               maxItemSetSize,
               maxNItemSets,
               enableParallel,
               frequentItemSet,
               accumulator)
        }

        itemId += 1
      }
    }

    accumulator
  }

  private def mineSinglePath[ItemType: ClassTag](
      node: Node,
      height: Int,
      headers: Array[Header[ItemType]],
      frequency: Int,
      minItemSetSize: Int,
      maxItemSetSize: Int,
      maxNItemSets: Int,
      enableParallel: Boolean,
      baseItemSet: FrequentItemSet[ItemType],
      accumulator: FrequentItemSetAccumulator[ItemType]
  ): Unit =
    if (maxItemSetSize == 0 || baseItemSet.size < maxItemSetSize) {
      //val parallel = fpTree.nNodes > 20 && enableParallel

      // at most this level of ancestor nodes of header.node can be the startNode to satisfy minItemSetSize
      var offset      = 1
      var currentNode = node
      while (offset <= height - 0.max(minItemSetSize - baseItemSet.size - 1)) {
        currentNode = currentNode.parent;

        val frequentItemSet = baseItemSet.add(headers(currentNode.itemId).item, frequency)
        if (frequentItemSet.size >= minItemSetSize && accumulator.size < maxNItemSets) {
          accumulator.add(frequentItemSet)
        }

        mineSinglePath(currentNode,
                       height - offset,
                       headers,
                       frequency,
                       minItemSetSize,
                       maxItemSetSize,
                       maxNItemSets,
                       enableParallel,
                       frequentItemSet,
                       accumulator)

        offset += 1
      }
    }

}
