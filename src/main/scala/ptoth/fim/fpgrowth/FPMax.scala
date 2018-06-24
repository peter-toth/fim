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

// TODO: shouldn't be a better name FPMaxTreeHeader?
class LeveledFPTreeHeader[ItemType](override val item: ItemType, override val frequency: Int)
    extends FPTreeHeader(item, frequency) {

  var level = 0

  override def prepend(node: FPNode, level: Int): Unit =
    if (this.node == null || level > this.level) {
      super.prepend(node, level)
      this.level = level
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

  def builder[ItemType](itemFrequencies: collection.Map[ItemType, Int],
                        minFrequency: Int): LeveledFPTreeBuilder[ItemType] =
    new LeveledFPTreeBuilder(itemFrequencies, minFrequency)

  def apply[ItemType: ClassTag](itemsets: Array[Array[ItemType]], minFrequency: Int): FPMax[ItemType] = {
    val itemFrequencies = mutable.Map.empty[ItemType, Int]
    itemsets.foreach(_.toSet[ItemType].foreach(item => itemFrequencies(item) = itemFrequencies.getOrElse(item, 0) + 1))

    var fpTreeBuilder = builder(itemFrequencies, minFrequency)

    itemsets.foreach(fpTreeBuilder.add(_, 1))

    val fpTree = fpTreeBuilder.tree

    // scalastyle:off null
    fpTreeBuilder = null
    // scalastyle:on

    new FPMax(fpTree, minFrequency)
  }

}

class FPMax[ItemType: ClassTag](fpTree: Tree[LeveledFPTreeHeader[ItemType]], minFrequency: Int) {

  def mineTo(
      minFrequency: Int = this.minFrequency,
      minItemSetSize: Int = 1,
      maxItemSetSize: Int = 0,
      maxNItemSets: Int = 1000000,
      enableParallel: Boolean = true,
      baseItemSet: Option[FrequentItemSet[ItemType]] = None,
      accumulator: FrequentItemSetAccumulator[ItemType]
  ): accumulator.type = {
    if (minFrequency < this.minFrequency)
      throw new Exception(s"minFrequency can't be lower than the minFrequency of the input FPTree")

    if (fpTree.isEmpty) {
      if (baseItemSet.nonEmpty && baseItemSet.get.frequency > minFrequency && baseItemSet.get.size >= minItemSetSize &&
          accumulator.size < maxNItemSets) {
        accumulator.add(baseItemSet.get)
      }
    } else {
      // this encoder is only used to construct the appropriate number of headers, and pass the minFrequency to the
      val dummyEncoder = new ItemEncoder[Int] {

        override val minFrequency: Int = FPMax.this.minFrequency

        override val itemFrequencies: Array[(Int, Int)] =
          fpTree.headers.map(_ /*header*/ => (0, 0 /*header.frequency*/ ))

        override def encodeItem(item: Int): Option[Int] = None

      }

      mine(
        fpTree,
        minFrequency,
        minItemSetSize,
        maxItemSetSize,
        maxNItemSets,
        enableParallel,
        baseItemSet.getOrElse(FrequentItemSet.empty),
        baseItemSet.map(_.frequency).getOrElse(0),
        dummyEncoder,
        Seq.empty,
        accumulator
      )
    }

    accumulator
  }

  def mine(
      minFrequency: Int = this.minFrequency,
      minItemSetSize: Int = 1,
      maxItemSetSize: Int = 0,
      maxNItemSets: Int = 1000000,
      enableParallel: Boolean = true,
      baseItemSet: Option[FrequentItemSet[ItemType]] = None,
  ): ListAccumulator[ItemType] =
    mineTo(minFrequency,
           minItemSetSize,
           maxItemSetSize,
           maxNItemSets,
           enableParallel,
           baseItemSet,
           new ListAccumulator[ItemType])

  private def mine(
      fpTree: Tree[LeveledFPTreeHeader[ItemType]],
      minFrequency: Int,
      minItemSetSize: Int,
      maxItemSetSize: Int,
      maxNItemSets: Int,
      enableParallel: Boolean,
      baseItemSet: FrequentItemSet[ItemType],
      frequency: Int,
      itemIdEncoder: ItemEncoder[Int],
      mfiTreeBuilders: Seq[(TreeBuilder[Int, MFINode, MFIHeader], Tree[LeveledFPTreeHeader[ItemType]], Int)],
      accumulator: FrequentItemSetAccumulator[ItemType]
  ): Unit =
    if (maxItemSetSize == 0 || baseItemSet.size < maxItemSetSize) {
      //val parallel = fpTree.nNodes > 20 && enableParallel

      if (fpTree.isEmpty) {
        mineMaximal(baseItemSet,
                    minItemSetSize,
                    maxNItemSets,
                    itemIdEncoder,
                    mfiTreeBuilders,
                    Seq.empty,
                    Seq.empty,
                    frequency,
                    accumulator)
      } else if (fpTree.isMaxHeight && fpTree.headers.last.node.frequency >= minFrequency) {
        mineMaximal(
          baseItemSet,
          minItemSetSize,
          maxNItemSets,
          itemIdEncoder,
          mfiTreeBuilders,
          fpTree.headers.map(_.item),
          fpTree.headers.indices,
          fpTree.headers.last.node.frequency,
          accumulator
        )
      } else {
        val mfiTreeBuilder = new TreeBuilder[Int, MFINode, MFIHeader](itemIdEncoder,
                                                                      (_, _, _) => new MFIHeader,
                                                                      (itemId, parent) => new MFINode(itemId, parent))

        if (mfiTreeBuilders.nonEmpty) {
          val (prevMFITreeBuilder, _, prevItemId) = mfiTreeBuilders.head

          Iterator
            .iterate(prevMFITreeBuilder.tree.headers(prevItemId).node)(_.sibling)
            .takeWhile(_ != null)
            .foreach { node =>
              val itemIdSet = Iterator
                .iterate(node.parent)(_.parent)
                .takeWhile(_ != null)
                .flatMap(node => itemIdEncoder.encodeItem(node.itemId))
                .toArray
                .sorted

              mfiTreeBuilder.addEncoded(itemIdSet, null)
            }
        }

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

            val conditionalItemIdEncoder = ContinuousArrayEncoder(itemIdAndFrequencies, minFrequency)

            val tail = conditionalItemIdEncoder.itemFrequencies.map(_._1).sorted :+ itemId

            if (!maximalityChecking(tail, mfiTreeBuilder.tree)) {
              var conditionalFPTreeBuilder =
                new TreeBuilder[Int, FPNode, LeveledFPTreeHeader[ItemType]](
                  conditionalItemIdEncoder,
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
                    .flatMap(node => conditionalItemIdEncoder.encodeItem(node.itemId))
                    .toArray
                    .sorted

                  conditionalFPTreeBuilder.addEncoded(itemIdSet, node.frequency)
                }

              val conditionalFPTree = conditionalFPTreeBuilder.tree

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
                header.frequency,
                conditionalItemIdEncoder,
                (mfiTreeBuilder, fpTree, itemId) +: mfiTreeBuilders,
                accumulator
              )

              prune = !conditionalFPTree.isEmpty && conditionalFPTree.height == itemId &&
              conditionalFPTree.headers.last.node.frequency >= minFrequency
            }
          }
      }
    }

  private def maximalityChecking(itemIdSet: Array[Int], mfiTree: Tree[MFIHeader]): Boolean = {

    def maximalityChecking(itemIdSet: Array[Int], itemIdIndex: Int, node: MFINode): Boolean =
      if (itemIdIndex == -1) {
        true
      } else if (node == null) {
        false
      } else if (itemIdSet(itemIdIndex) < node.itemId) {
        maximalityChecking(itemIdSet, itemIdIndex, node.parent)
      } else if (itemIdSet(itemIdIndex) == node.itemId) {
        maximalityChecking(itemIdSet, itemIdIndex - 1, node.parent)
      } else {
        false
      }

    Iterator
      .iterate(mfiTree.headers(itemIdSet.last).node)(_.sibling)
      .takeWhile(_ != null)
      .exists(maximalityChecking(itemIdSet, itemIdSet.length - 1, _))
  }

  private def mineMaximal(
      baseItemSet: FrequentItemSet[ItemType],
      minItemSetSize: Int,
      maxNItemSets: Int,
      itemIdEncoder: ItemEncoder[Int],
      mfiTreeBuilders: Seq[(TreeBuilder[Int, MFINode, MFIHeader], Tree[LeveledFPTreeHeader[ItemType]], Int)],
      itemSet: Seq[ItemType],
      itemIdSet: Seq[Int],
      frequency: Int,
      accumulator: FrequentItemSetAccumulator[ItemType]
  ) {
    var currentItemIdSet     = itemIdSet
    var currentItemIdEncoder = itemIdEncoder
    var currentItemSet       = itemSet
    mfiTreeBuilders.foreach {
      case (mfiTreeBuilder, conditionalFPTree, itemId) =>
        val decodedCurrentItemIdSet = currentItemIdSet.flatMap(currentItemIdEncoder.decodeItem)
        mfiTreeBuilder.addEncoded(decodedCurrentItemIdSet.toArray.sorted, null)
        currentItemIdSet = itemId +: decodedCurrentItemIdSet
        currentItemIdEncoder = mfiTreeBuilder.itemEncoder
        currentItemSet +:= conditionalFPTree.headers(itemId).item
    }

    // TODO: addItems is costly, check if we really need to do it
    val frequentItemSet = baseItemSet.addItems(currentItemSet.toArray, frequency)
    if (frequentItemSet.size >= minItemSetSize && accumulator.size < maxNItemSets) {
      accumulator.add(frequentItemSet)
    }
  }

}
