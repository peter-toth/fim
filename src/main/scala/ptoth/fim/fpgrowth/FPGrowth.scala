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

import ptoth.fim._
import ptoth.fim.common._

import scala.collection.mutable
import scala.reflect.ClassTag

class FPNode(override val itemId: Int, override val parent: FPNode) extends Node[FPNode, Int](itemId, parent) {

  var frequency: Int = 0

  override def update(frequency: Int): Unit =
    this.frequency += frequency

}

class FPHeader[ItemType](val item: ItemType, val frequency: Int) extends Header[FPNode, Int]

class FPTreeBuilder[ItemType](itemEncoder: ItemEncoder[ItemType])
    extends TreeBuilder[ItemType, FPNode, Int, FPHeader[ItemType]](
      itemEncoder,
      (_, item, frequency) => new FPHeader(item, frequency),
      (itemId, parent) => new FPNode(itemId, parent)
    )

object FPGrowth {

  def apply[ItemType: ClassTag](
      itemsets: Array[Array[ItemType]],
      minFrequency: Int,
      minItemSetSize: Int = 1,
      maxItemSetSize: Int = 0,
      maxNItemSets: Int = 1000000,
      enableParallel: Boolean = true,
      baseItemSet: Option[FrequentItemSet[ItemType]] = None,
      accumulator: FrequentItemSetAccumulator[ItemType] = SetAccumulator[ItemType]()
  ): accumulator.type = {
    val itemFrequencies = mutable.Map.empty[ItemType, Int]
    itemsets.foreach(_.toSet[ItemType].foreach(item => itemFrequencies(item) = itemFrequencies.getOrElse(item, 0) + 1))

    val itemEncoder   = new MapEncoder(itemFrequencies, minFrequency)
    var fpTreeBuilder = new FPTreeBuilder(itemEncoder)

    itemsets.foreach(fpTreeBuilder.add(_, 1))

    val fPTree = fpTreeBuilder.fpTree

    // scalastyle:off null
    fpTreeBuilder = null
    // scalastyle:on

    mine[ItemType](fPTree,
                   minFrequency,
                   minItemSetSize,
                   maxItemSetSize,
                   maxNItemSets,
                   enableParallel,
                   baseItemSet.getOrElse(FrequentItemSet.empty[ItemType]),
                   accumulator)

    accumulator
  }

  def mine[ItemType: ClassTag](
      fpTree: Tree[FPNode, Int, FPHeader[ItemType]],
      minFrequency: Int,
      minItemSetSize: Int = 1,
      maxItemSetSize: Int = 0,
      maxNItemSets: Int = 1000000,
      enableParallel: Boolean = true,
      baseItemSet: Option[FrequentItemSet[ItemType]] = None,
      accumulator: FrequentItemSetAccumulator[ItemType] = SetAccumulator[ItemType]()
  ): accumulator.type = {
    mine(fpTree,
         minFrequency,
         minItemSetSize,
         maxItemSetSize,
         maxNItemSets,
         enableParallel,
         baseItemSet.getOrElse(FrequentItemSet.empty),
         accumulator)

    accumulator
  }

  private def mine[ItemType: ClassTag](
      fpTree: Tree[FPNode, Int, FPHeader[ItemType]],
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

        val frequentItemSet: FrequentItemSet[ItemType] = baseItemSet.addItem(header.item, header.frequency)
        if (frequentItemSet.size >= minItemSetSize && accumulator.size < maxNItemSets) {
          accumulator.add(frequentItemSet)
        }

        if (header.node.sibling == null) {
          var height =
            Iterator.iterate(header.node.parent)(_.parent).takeWhile(_ != null).foldLeft(0)((height, _) => height + 1)

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
          val oldItemIdAndFrequencies = new Array[Int](fpTree.headers.size);

          Iterator
            .iterate(fpTree.headers(itemId).node)(_.sibling)
            .takeWhile(_ != null)
            .foreach(
              node =>
                Iterator
                  .iterate(node.parent)(_.parent)
                  .takeWhile(_ != null)
                  .foreach(
                    currentNode => oldItemIdAndFrequencies(currentNode.itemId) += node.frequency
                )
            )

          val oldItemIdEncoder = ContinuousArrayEncoder(oldItemIdAndFrequencies, minFrequency)

          val conditionalFPTreeBuilder =
            new TreeBuilder[Int, FPNode, Int, FPHeader[ItemType]](
              oldItemIdEncoder,
              (_, oldItemId, frequency) => new FPHeader(fpTree.headers(oldItemId).item, frequency),
              (itemId, parent) => new FPNode(itemId, parent)
            )
          Iterator
            .iterate(fpTree.headers(itemId).node)(_.sibling)
            .takeWhile(_ != null)
            .foreach { node =>
              val itemIdSet = Iterator
                .iterate(node)(_.parent)
                .takeWhile(_ != null)
                .flatMap(node => oldItemIdEncoder.encodeItem(node.itemId))
                .toArray
                .sorted

              conditionalFPTreeBuilder.addEncoded(itemIdSet, node.frequency)
            }

          val conditionalFPTree = conditionalFPTreeBuilder.fpTree

          mine(conditionalFPTree,
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
      node: FPNode,
      height: Int,
      headers: Array[FPHeader[ItemType]],
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

        val frequentItemSet = baseItemSet.addItem(headers(currentNode.itemId).item, frequency)
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
