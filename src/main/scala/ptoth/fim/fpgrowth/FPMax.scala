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

import ptoth.fim.{ FrequentItemSet, _ }
import ptoth.fim.common._

import scala.collection.mutable
import scala.reflect.ClassTag

class LeveledFPTreeHeader[ItemType](override val item: ItemType, override val frequency: Int)
    extends FPTreeHeader(item, frequency) {

  var level = 0

  override def prepend(node: FPNode, level: Int): Unit =
    if (this.node == null || level > this.level) {
      super.prepend(node, level)
    } else {
      node.sibling = this.node.sibling
      this.node.sibling = node
    }

}

class LeveledFPTreeBuilder[ItemType](itemFrequencies: collection.Map[ItemType, Int], minFrequency: Int)
    extends TreeBuilder[ItemType, FPNode, LeveledFPTreeHeader[ItemType]](
      MapEncoder(itemFrequencies, minFrequency),
      (_, item, frequency) => new LeveledFPTreeHeader(item, frequency),
      (itemId, parent) => new FPNode(itemId, parent)
    )

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
      accumulator: FrequentItemSetAccumulator[ItemType] = ListAccumulator[ItemType]()
  ): accumulator.type = {
    val itemFrequencies = mutable.Map.empty[ItemType, Int]
    itemsets.foreach(_.toSet[ItemType].foreach(item => itemFrequencies(item) = itemFrequencies.getOrElse(item, 0) + 1))

    var fpTreeBuilder = new LeveledFPTreeBuilder(itemFrequencies, minFrequency)

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
      fpTree: Tree[LeveledFPTreeHeader[ItemType]],
      minFrequency: Int,
      minItemSetSize: Int = 1,
      maxItemSetSize: Int = 0,
      maxNItemSets: Int = 1000000,
      enableParallel: Boolean = true,
      baseItemSet: Option[FrequentItemSet[ItemType]] = None,
      accumulator: FrequentItemSetAccumulator[ItemType] = ListAccumulator[ItemType]()
  ): accumulator.type = {
    if (fpTree.isEmpty) {
      if (baseItemSet.size >= minItemSetSize && accumulator.size < maxNItemSets) {
        accumulator.add(baseItemSet.get) // TODO: check base frequency
      }
    } else {
      val dummyEncoder = new ItemEncoder[ItemType] {

        override val itemFrequencies: Array[(ItemType, Int)] =
          fpTree.headers.map(header => (header.item, header.frequency))

        private val itemToItemId = itemFrequencies.map(_._1).zipWithIndex.toMap

        override def encodeItem(item: ItemType): Option[Int] = itemToItemId.get(item)

      }

      val mfiTreeBuilder = new TreeBuilder[ItemType, MFINode, MFIHeader](
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
        Seq.empty,
        0,
        mfiTreeBuilder,
        accumulator
      )
    }

    accumulator
  }

  private def mine[ItemType: ClassTag](
      fpTree: Tree[LeveledFPTreeHeader[ItemType]],
      minFrequency: Int,
      minItemSetSize: Int,
      maxItemSetSize: Int,
      maxNItemSets: Int,
      enableParallel: Boolean,
      baseItemSet: FrequentItemSet[ItemType],
      baseItems: Seq[ItemType],
      baseItemIds: Seq[Int],
      frequency: Int,
      mfiTreeBuilder: TreeBuilder[ItemType, MFINode, MFIHeader],
      accumulator: FrequentItemSetAccumulator[ItemType]
  ): Unit =
    if (maxItemSetSize == 0 || baseItemSet.size < maxItemSetSize) {
      //val parallel = fpTree.nNodes > 20 && enableParallel

      if (fpTree.isEmpty) {
        mineMaximal(baseItemIds.toArray.sorted,
                    baseItemSet.addItems(baseItems.toArray, frequency),
                    minItemSetSize,
                    maxNItemSets,
                    mfiTreeBuilder,
                    accumulator)
      } else if (fpTree.isMaxHeight && fpTree.headers.last.node.frequency > minFrequency) {
        val tail = fpTree.headers.map(_.item)

        mineMaximal(
          (mfiTreeBuilder.itemEncoder.encodeItems(tail) ++ baseItemIds).sorted,
          baseItemSet.addItems(tail ++ baseItems, fpTree.headers.last.node.frequency),
          minItemSetSize,
          maxNItemSets,
          mfiTreeBuilder,
          accumulator
        )
      } else {
        var prune = false

        Iterator
          .iterate(fpTree.headers.length - 1)(_ - 1)
          .takeWhile(!prune && _ >= 0.max(minItemSetSize - baseItemSet.size - 1))
          .foreach { itemId =>
            val header = fpTree.headers(itemId)

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

            val tail = header.item +: itemIdEncoder.itemFrequencies.map {
              case (itemId, _) => fpTree.headers(itemId).item
            }

            if (!subsetChecking(tail, baseItemIds, mfiTreeBuilder)) {
              var conditionalFPTreeBuilder =
                new TreeBuilder[Int, FPNode, LeveledFPTreeHeader[ItemType]](
                  itemIdEncoder,
                  (_, itemId, frequency) => new LeveledFPTreeHeader(fpTree.headers(itemId).item, frequency),
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
              prune = !conditionalFPTree.isEmpty && conditionalFPTree.height == itemId && conditionalFPTree.headers.last.node.frequency > minFrequency

              // scalastyle:off null
              conditionalFPTreeBuilder = null
              // scalastyle:on

              mine(
                conditionalFPTree,
                minFrequency,
                minItemSetSize,
                maxItemSetSize,
                maxNItemSets,
                enableParallel,
                baseItemSet,
                header.item +: baseItems,
                mfiTreeBuilder.itemEncoder.encodeItem(header.item).get +: baseItemIds,
                header.frequency,
                mfiTreeBuilder,
                accumulator
              )
            }
          }
      }
    }

  private def subsetChecking[ItemType](
      itemSet: Array[ItemType],
      baseItemIdSet: Seq[Int],
      mfiTreeBuilder: TreeBuilder[ItemType, MFINode, MFIHeader]
  ): Boolean = {

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

    val itemIdSet = (mfiTreeBuilder.itemEncoder.encodeItems(itemSet) ++ baseItemIdSet).sorted

    Iterator
      .iterate(mfiTreeBuilder.tree.headers(itemIdSet.last).node)(_.sibling)
      .takeWhile(_ != null)
      .exists(subsetChecking(itemIdSet, itemIdSet.length - 1, _))
  }

  private def mineMaximal[ItemType](itemSetIds: Array[Int],
                                    frequentItemSet: FrequentItemSet[ItemType],
                                    minItemSetSize: Int,
                                    maxNItemSets: Int,
                                    mfiTreeBuilder: TreeBuilder[ItemType, MFINode, MFIHeader],
                                    accumulator: FrequentItemSetAccumulator[ItemType]) {
    mfiTreeBuilder.addEncoded(itemSetIds, null)

    if (frequentItemSet.size >= minItemSetSize && accumulator.size < maxNItemSets) {
      accumulator.add(frequentItemSet) // TODO: do the check before and do not create frequentItemSet if it fails
    }
  }

}
