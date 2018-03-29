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

//class FPNode(override val itemId: Int, override val parent: FPNode) extends Node[FPNode](itemId, parent)

class FPHeader[ItemType](val item: ItemType, val frequency: Int) extends Header

class FPTreeBuilder[ItemType](itemEncoder: ItemEncoder[ItemType])
    extends TreeBuilder[ItemType, FPHeader[ItemType]](itemEncoder,
                                                      (_, item, frequency) => new FPHeader(item, frequency),
                                                      (itemId, parent) => new Node(itemId, parent))

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
    val t0 = System.currentTimeMillis

    val itemFrequencies = mutable.Map.empty[ItemType, Int]
    itemsets.foreach(_.toSet[ItemType].foreach(item => itemFrequencies(item) = itemFrequencies.getOrElse(item, 0) + 1))

    val itemEncoder   = new MapEncoder(itemFrequencies, minFrequency)
    val fpTreeBuilder = new FPTreeBuilder(itemEncoder)

    itemsets.foreach(fpTreeBuilder.add(_, 1))

    val t1 = System.currentTimeMillis

    Measure.nTreeBuilding += 1
    Measure.tTreeBuilding += (t1 - t0)

    val fPTree = fpTreeBuilder.fpTree

    // TODO: destroy fpTreeBuilder to free up some memory

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
      fpTree: Tree[FPHeader[ItemType]],
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
      fpTree: Tree[FPHeader[ItemType]],
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

          val t0 = System.currentTimeMillis

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

          val t1 = System.currentTimeMillis

          Measure.nSinglePathMining += 1
          Measure.tSinglePathMining += (t1 - t0)

        } else {
          val t0 = System.currentTimeMillis

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

          val t1 = System.currentTimeMillis

          Measure.nFrequencyCounting += 1
          Measure.tFrequencyCounting += (t1 - t0)

          val oldItemIdEncoder = ContinuousArrayEncoder(oldItemIdAndFrequencies, minFrequency)

          val conditionalFPTreeBuilder =
            new TreeBuilder[Int, FPHeader[ItemType]](
              oldItemIdEncoder,
              (_, oldItemId, frequency) => new FPHeader(fpTree.headers(oldItemId).item, frequency),
              (itemId, parent) => new Node(itemId, parent)
            )

          val t3 = System.currentTimeMillis

          Measure.nHeaderBuilding += 1
          Measure.tHeaderBuilding += (t3 - t1)

          Iterator
            .iterate(fpTree.headers(itemId).node)(_.sibling)
            .takeWhile(_ != null)
            .foreach { node =>
              val t8 = System.currentTimeMillis

              val oldItemIdSet = Iterator
                .iterate(node)(_.parent)
                .takeWhile(_ != null)
                .flatMap(node => oldItemIdEncoder.encodeItem(node.itemId))
                .toArray
                .sorted

              val t9 = System.currentTimeMillis

              Measure.nOldItemSetGeneration += 1
              Measure.tOldItemSetGeneration += (t9 - t8)

              conditionalFPTreeBuilder.addEncoded(oldItemIdSet, node.frequency)
            }

          val t4 = System.currentTimeMillis

          Measure.nConditionalTreeBuilding += 1
          Measure.tConditionalTreeBuilding += (t4 - t3)

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
      node: Node,
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

object Measure {

  var tFrequencyCounting: Long       = 0
  var nFrequencyCounting: Int        = 0
  var tConditionalTreeBuilding: Long = 0
  var nConditionalTreeBuilding: Int  = 0
  var tSinglePathMining: Long        = 0
  var nSinglePathMining: Int         = 0
  var tTreeBuilding: Long            = 0
  var nTreeBuilding: Int             = 0
  var tHeaderBuilding: Long          = 0
  var nHeaderBuilding: Int           = 0
  var tOldItemSetGeneration: Long    = 0
  var nOldItemSetGeneration: Int     = 0

  override def toString: String =
    s"tFrequencyCounting: $tFrequencyCounting\n" +
    s"nFrequencyCounting: $nFrequencyCounting\n" +
    s"tConditionalTreeBuilding: $tConditionalTreeBuilding\n" +
    s"nConditionalTreeBuilding: $nConditionalTreeBuilding\n" +
    s"tSinglePathMining: $tSinglePathMining\n" +
    s"nSinglePathMining: $nSinglePathMining\n" +
    s"tTreeBuilding: $tTreeBuilding\n" +
    s"nTreeBuilding: $nTreeBuilding\n" +
    s"tHeaderBuilding: $tHeaderBuilding\n" +
    s"nHeaderBuilding: $nHeaderBuilding\n" +
    s"tOldItemSetGeneration: $tOldItemSetGeneration\n" +
    s"nOldItemSetGeneration: $nOldItemSetGeneration\n"

}
