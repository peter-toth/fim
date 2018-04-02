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

//class FPMaxNode(override val itemId: Int, override val parent: FPMaxNode) extends Node[FPMaxNode](itemId, parent) {
//
//  override type DataType = Int
//
//  var frequency: Int = 0
//
//  override def update(data: DataType): Unit =
//    frequency += data
//
//}
//
//class FPMaxHeader[ItemType](val item: ItemType, val oritinalItemId: Int, val frequency: Int) extends Header[FPMaxNode]
//
//class FPMaxBuilder[ItemType](itemFrequencies: collection.Map[ItemType, Int], minFrequency: Int)
//    extends TreeBuilder[ItemType, FPMaxNode, FPMaxHeader[ItemType]](
//      new MapEncoder(itemFrequencies, minFrequency),
//      (_, item, frequency) => new FPGrowthHeader(item, frequency),
//      (itemId, parent) => new FPGrowthNode(itemId, parent)
//    )

class MFINode(override val itemId: Int, override val parent: MFINode) extends Node[MFINode](itemId, parent) {

  override type DataType = Null

  override def update(data: Null): Unit = {}

  val level: Int = if (parent == null) 0 else parent.level + 1

}

class MFIHeader extends Header[MFINode]

object FPMax {

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

    var fpTreeBuilder = new FPTreeBuilder(itemFrequencies, minFrequency)

    itemsets.foreach(fpTreeBuilder.add(_, 1))

    val fPTree = fpTreeBuilder.tree

    // scalastyle:off null
    fpTreeBuilder = null
    // scalastyle:on

    mine[ItemType](fPTree,
                   minFrequency,
                   minItemSetSize,
                   maxItemSetSize,
                   maxNItemSets,
                   enableParallel,
                   baseItemSet,
                   accumulator)

    accumulator
  }

  def mine[ItemType: ClassTag](
      fpTree: Tree[FPTreeHeader[ItemType]],
      minFrequency: Int,
      minItemSetSize: Int = 1,
      maxItemSetSize: Int = 0,
      maxNItemSets: Int = 1000000,
      enableParallel: Boolean = true,
      baseItemSet: Option[FrequentItemSet[ItemType]] = None,
      accumulator: FrequentItemSetAccumulator[ItemType] = SetAccumulator[ItemType]()
  ): accumulator.type = {
    val itemIdToOriginalItemId = Array.range(0, fpTree.headers.length)

    val dummyEncoder = new ItemEncoder[Int] {

      override val itemFrequencies: Array[(Int, Int)] = fpTree.headers.map(_.frequency).zipWithIndex.map(_.swap)

      override def encodeItem(item: Int): Option[Int] = Some(item)

    }

    val mfiTreeBuilder = new TreeBuilder[Int, MFINode, MFIHeader](
      dummyEncoder,
      (_, _, _) => new MFIHeader,
      (itemId, parent) => new MFINode(itemId, parent)
    )

    mine(
      fpTree,
      minFrequency,
      minItemSetSize,
      maxItemSetSize,
      maxNItemSets,
      enableParallel,
      baseItemSet.getOrElse(FrequentItemSet.empty),
      Seq.empty,
      itemIdToOriginalItemId,
      mfiTreeBuilder,
      accumulator
    )

    accumulator
  }

  private def mine[ItemType: ClassTag](
      fpTree: Tree[FPTreeHeader[ItemType]],
      minFrequency: Int,
      minItemSetSize: Int,
      maxItemSetSize: Int,
      maxNItemSets: Int,
      enableParallel: Boolean,
      baseItemSet: FrequentItemSet[ItemType],
      baseOriginalItemIdSet: Seq[Int],
      itemIdToOriginalItemId: Array[Int],
      mfiTreeBuilder: TreeBuilder[Int, MFINode, MFIHeader],
      accumulator: FrequentItemSetAccumulator[ItemType]
  ): accumulator.type = {
    if (maxItemSetSize == 0 || baseItemSet.size < maxItemSetSize) {
      //val parallel = fpTree.nNodes > 20 && enableParallel

      if (fpTree.isEmpty) {
        if (baseOriginalItemIdSet.nonEmpty) {
          val originalItemIdSet = baseOriginalItemIdSet.toArray.sorted

          mfiTreeBuilder.addEncoded(originalItemIdSet, null)
        }
        if (!baseItemSet.isEmpty) {
          if (baseItemSet.size >= minItemSetSize && accumulator.size < maxNItemSets) {
            accumulator.add(baseItemSet)
          }
        }
      } else if (fpTree.singlePath) {
        val header = fpTree.headers.last

        val originalItemIdSet = (Iterator
          .iterate(header.node)(_.parent)
          .takeWhile(_ != null)
          .map(node => itemIdToOriginalItemId(node.itemId)) ++ baseOriginalItemIdSet).toArray.sorted

        mfiTreeBuilder.addEncoded(originalItemIdSet, null)

        val items = Iterator
          .iterate(fpTree.headers.last.node)(_.parent)
          .takeWhile(_ != null)
          .map(node => fpTree.headers(node.itemId).item)
          .toArray

        val frequentItemSet: FrequentItemSet[ItemType] = baseItemSet.addItems(items, header.frequency)
        if (frequentItemSet.size >= minItemSetSize && accumulator.size < maxNItemSets) {
          accumulator.add(frequentItemSet)
        }
      } else {
        var itemId = fpTree.headers.length - 1
        while (itemId >= 0.max(minItemSetSize - baseItemSet.size - 1)) {
          val header = fpTree.headers(itemId)

          val itemIdAndFrequencies = new Array[Int](fpTree.headers.length)
          Iterator
            .iterate(fpTree.headers(itemId).node)(_.sibling)
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

          val conditionalItemIdToOriginalItemId = itemIdEncoder.itemFrequencies.map {
            case (iId, _) => itemIdToOriginalItemId(iId)
          }

          val conditionalBaseOriginalItemIdSet = itemIdToOriginalItemId(itemId) +: baseOriginalItemIdSet

          val originalItemIdSet = (conditionalItemIdToOriginalItemId ++ conditionalBaseOriginalItemIdSet).sorted

          if (!subsetChecking(originalItemIdSet, mfiTreeBuilder.tree)) {
            val conditionalFPTreeBuilder =
              new TreeBuilder[Int, FPNode, FPTreeHeader[ItemType]](
                itemIdEncoder,
                (_, itemId, frequency) => new FPTreeHeader(fpTree.headers(itemId).item, frequency),
                (itemId, parent) => new FPNode(itemId, parent)
              )
            Iterator
              .iterate(fpTree.headers(itemId).node)(_.sibling)
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

            mine(
              conditionalFPTree,
              minFrequency,
              minItemSetSize,
              maxItemSetSize,
              maxNItemSets,
              enableParallel,
              baseItemSet.addItem(header.item, header.frequency),
              conditionalBaseOriginalItemIdSet,
              conditionalItemIdToOriginalItemId,
              mfiTreeBuilder,
              accumulator
            )
          }

          itemId -= 1
        }
      }
    }

    accumulator
  }

  private def subsetChecking(itemIdSet: Array[Int], mfiTree: Tree[MFIHeader]): Boolean = {

    def subsetChecking(itemIdSet: Array[Int], itemIdIndex: Int, node: MFINode): Boolean =
      if (itemIdIndex == -1) {
        true
      } else if (node == null) {
        false
      } else if (itemIdSet(itemIdIndex) < node.itemId) {
        subsetChecking(itemIdSet, itemIdIndex, node.parent)
      } else if (itemIdSet(itemIdIndex) == node.itemId) {
        subsetChecking(itemIdSet, itemIdIndex - 1, node.parent)
      } else {
        false
      }

    Iterator
      .iterate(mfiTree.headers(itemIdSet.last).node)(_.sibling)
      .takeWhile(_ != null)
      .exists(subsetChecking(itemIdSet, itemIdSet.length - 1, _))
  }

}
