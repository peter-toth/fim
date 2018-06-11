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

class FPNode(override val itemId: Int, override val parent: FPNode) extends Node[FPNode](itemId, parent) {

  override type DataType = Int

  var frequency: Int = 0

  override def update(data: DataType): Unit =
    frequency += data

}

class FPTreeHeader[ItemType](val item: ItemType, val frequency: Int) extends Header[FPNode] {

  override def toString: String =
    s"item: $item\t" + Iterator.iterate(node)(_.sibling).takeWhile(_ != null).mkString("\t")

}

class FPTreeBuilder[ItemType](itemFrequencies: collection.Map[ItemType, Int], minFrequency: Int)
    extends TreeBuilder[ItemType, FPNode, FPTreeHeader[ItemType]](
      MapEncoder(itemFrequencies, minFrequency),
      (_, item, frequency) => new FPTreeHeader(item, frequency),
      (itemId, parent) => new FPNode(itemId, parent)
    )

object FPGrowth {

  def builder[ItemType](itemFrequencies: collection.Map[ItemType, Int], minFrequency: Int): FPTreeBuilder[ItemType] =
    new FPTreeBuilder(itemFrequencies, minFrequency)

  def apply[ItemType: ClassTag](itemsets: Array[Array[ItemType]], minFrequency: Int): FPGrowth[ItemType] = {
    val itemFrequencies = mutable.Map.empty[ItemType, Int]
    itemsets.foreach(_.toSet[ItemType].foreach(item => itemFrequencies(item) = itemFrequencies.getOrElse(item, 0) + 1))

    var fpTreeBuilder = builder(itemFrequencies, minFrequency)

    itemsets.foreach(fpTreeBuilder.add(_, 1))

    val fpTree = fpTreeBuilder.tree

    // scalastyle:off null
    fpTreeBuilder = null
    // scalastyle:on

    new FPGrowth(fpTree, minFrequency)
  }

}

class FPGrowth[ItemType: ClassTag](fpTree: Tree[FPTreeHeader[ItemType]], minFrequency: Int) {

  def mineTo(
      minFrequency: Int = this.minFrequency,
      minItemSetSize: Int = 1,
      maxItemSetSize: Int = 0,
      maxNItemSets: Int = 1000000,
      enableParallel: Boolean = true,
      baseItemSet: Option[FrequentItemSet[ItemType]] = None,
      accumulator: FrequentItemSetAccumulator[ItemType] = ListAccumulator[ItemType]()
  ): accumulator.type = {
    if (minFrequency < this.minFrequency)
      throw new Exception(s"minFrequency can't be lower than the minFrequency of the input FPTree")

    if (baseItemSet.nonEmpty && baseItemSet.get.frequency > minFrequency && baseItemSet.get.size >= minItemSetSize && accumulator.size < maxNItemSets) {
      accumulator.add(baseItemSet.get)
    }

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

  def mine(
      minFrequency: Int = this.minFrequency,
      minItemSetSize: Int = 1,
      maxItemSetSize: Int = 0,
      maxNItemSets: Int = 1000000,
      enableParallel: Boolean = true,
      baseItemSet: Option[FrequentItemSet[ItemType]] = None
  ): ListAccumulator[ItemType] =
    mineTo(minFrequency,
           minItemSetSize,
           maxItemSetSize,
           maxNItemSets,
           enableParallel,
           baseItemSet,
           new ListAccumulator[ItemType])

  private def mine(
      fpTree: Tree[FPTreeHeader[ItemType]],
      minFrequency: Int,
      minItemSetSize: Int,
      maxItemSetSize: Int,
      maxNItemSets: Int,
      enableParallel: Boolean,
      baseItemSet: FrequentItemSet[ItemType],
      accumulator: FrequentItemSetAccumulator[ItemType]
  ): Unit =
    if (maxItemSetSize == 0 || baseItemSet.size < maxItemSetSize) {
      //val parallel = fpTree.nNodes > 20 && enableParallel

      if (!fpTree.isEmpty) {
        if (fpTree.singlePath) {
          val header = fpTree.headers.last

          mineSinglePath(header.node,
                         header.node.height,
                         fpTree.headers,
                         header.frequency,
                         minItemSetSize,
                         maxItemSetSize,
                         maxNItemSets,
                         enableParallel,
                         baseItemSet,
                         accumulator)
        } else {
          Iterator.range(0.max(minItemSetSize - baseItemSet.size - 1), fpTree.headers.length).foreach { itemId =>
            val header = fpTree.headers(itemId)

            val frequentItemSet: FrequentItemSet[ItemType] = baseItemSet.addItem(header.item, header.frequency)
            if (frequentItemSet.size >= minItemSetSize && accumulator.size < maxNItemSets) {
              accumulator.add(frequentItemSet)
            }

            if (header.node.sibling == null) {
              if (header.node.parent != null) {
                mineSinglePath(
                  header.node.parent,
                  header.node.parent.height,
                  fpTree.headers,
                  header.frequency,
                  minItemSetSize,
                  maxItemSetSize,
                  maxNItemSets,
                  enableParallel,
                  frequentItemSet,
                  accumulator
                )
              }
            } else {
              val itemIdAndFrequencies = new Array[Int](fpTree.headers.length)

              Iterator
                .iterate(header.node)(_.sibling)
                .takeWhile(_ != null)
                .foreach(
                  node =>
                    Iterator
                      .iterate(node.parent)(_.parent)
                      .takeWhile(_ != null)
                      .foreach(
                        currentNode => itemIdAndFrequencies(currentNode.itemId) += node.frequency
                    )
                )

              val itemIdEncoder = ContinuousArrayEncoder(itemIdAndFrequencies, minFrequency)

              var conditionalFPTreeBuilder =
                new TreeBuilder[Int, FPNode, FPTreeHeader[ItemType]](
                  itemIdEncoder,
                  (_, oldItemId, frequency) => new FPTreeHeader(fpTree.headers(oldItemId).item, frequency),
                  (itemId, parent) => new FPNode(itemId, parent)
                )

              Iterator
                .iterate(header.node)(_.sibling)
                .takeWhile(_ != null)
                .foreach { node =>
                  val itemIdSet = Iterator
                    .iterate(node)(_.parent)
                    .takeWhile(_ != null)
                    .flatMap(node => itemIdEncoder.encodeItem(node.itemId))
                    .toArray
                    .sorted

                  conditionalFPTreeBuilder.addEncoded(itemIdSet, node.frequency)
                }

              val conditionalFPTree = conditionalFPTreeBuilder.tree

              // scalastyle:off null
              conditionalFPTreeBuilder = null
              // scalastyle:on

              mine(conditionalFPTree,
                   minFrequency,
                   minItemSetSize,
                   maxItemSetSize,
                   maxNItemSets,
                   enableParallel,
                   frequentItemSet,
                   accumulator)
            }
          }
        }
      }
    }

  private def mineSinglePath(
      node: FPNode,
      height: Int,
      headers: Array[FPTreeHeader[ItemType]],
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
      var currentNode = node
      Iterator.range(1, height - 0.max(minItemSetSize - baseItemSet.size - 1) + 1).foreach { level =>
        val frequentItemSet = baseItemSet.addItem(headers(currentNode.itemId).item, frequency)
        if (frequentItemSet.size >= minItemSetSize && accumulator.size < maxNItemSets) {
          accumulator.add(frequentItemSet)
        }

        currentNode = currentNode.parent

        mineSinglePath(currentNode,
                       height - level,
                       headers,
                       frequency,
                       minItemSetSize,
                       maxItemSetSize,
                       maxNItemSets,
                       enableParallel,
                       frequentItemSet,
                       accumulator)
      }
    }

}
