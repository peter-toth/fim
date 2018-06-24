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

import ptoth.fim.common._
import ptoth.fim.{ FrequentItemSet, _ }

import scala.collection.mutable
import scala.reflect.ClassTag

class CFINode(override val itemId: Int, override val parent: CFINode) extends Node[CFINode](itemId, parent) {

  override type DataType = Int

  var frequency: Int = 0

  override def update(data: DataType): Unit = frequency = frequency.max(data) // TODO: is max needed here?

  val level: Int = if (parent == null) 0 else parent.level + 1

}

class CFIHeader extends Header[CFINode]

object FPClose {

  def builder[ItemType](itemFrequencies: collection.Map[ItemType, Int],
                        minFrequency: Int): LeveledFPTreeBuilder[ItemType] =
    new LeveledFPTreeBuilder(itemFrequencies, minFrequency)

  def apply[ItemType: ClassTag](itemsets: Array[Array[ItemType]], minFrequency: Int): FPClose[ItemType] = {
    val itemFrequencies = mutable.Map.empty[ItemType, Int]
    itemsets.foreach(_.toSet[ItemType].foreach(item => itemFrequencies(item) = itemFrequencies.getOrElse(item, 0) + 1))

    var fpTreeBuilder = builder(itemFrequencies, minFrequency)

    itemsets.foreach(fpTreeBuilder.add(_, 1))

    val fpTree = fpTreeBuilder.tree

    // scalastyle:off null
    fpTreeBuilder = null
    // scalastyle:on

    new FPClose(fpTree, minFrequency)
  }

  type a = Int

}

class FPClose[ItemType: ClassTag](fpTree: Tree[LeveledFPTreeHeader[ItemType]], minFrequency: Int) {

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

        override val minFrequency: Int = FPClose.this.minFrequency

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
      cfiTreeBuilders: Seq[(TreeBuilder[Int, CFINode, CFIHeader], Tree[LeveledFPTreeHeader[ItemType]], Int)],
      accumulator: FrequentItemSetAccumulator[ItemType]
  ): Unit =
    if (maxItemSetSize == 0 || baseItemSet.size < maxItemSetSize) {
      //val parallel = fpTree.nNodes > 20 && enableParallel

      if (fpTree.isEmpty) {
        mineClosed(baseItemSet,
                   minItemSetSize,
                   maxNItemSets,
                   itemIdEncoder,
                   cfiTreeBuilders,
                   Seq.empty,
                   Seq.empty,
                   frequency,
                   accumulator)
      } else {
        if (cfiTreeBuilders.nonEmpty &&
            frequency > fpTree.headers.head.frequency &&
            !closedChecking(Array(cfiTreeBuilders.head._3), frequency, cfiTreeBuilders.head._1.tree)) {
          mineClosed(baseItemSet,
                     minItemSetSize,
                     maxNItemSets,
                     itemIdEncoder,
                     cfiTreeBuilders,
                     Seq.empty,
                     Seq.empty,
                     frequency,
                     accumulator)
        }

        if (fpTree.isSinglePath) { // TODO: handling of single path trees can be more efficient without building the conditional tree
          Iterator // TODO: do we need to reorder and put lower frequencies first?
            .iterate(0)(_ + 1)
            .takeWhile(_ < fpTree.headers.length)
            .foreach { itemId =>
              if (itemId == fpTree.headers.length - 1
                  || fpTree.headers(itemId).frequency != fpTree.headers(itemId + 1).frequency) {
                val candidateClosedHeaders = fpTree.headers.take(itemId + 1)

                if (cfiTreeBuilders.isEmpty
                    || !closedChecking(
                      (candidateClosedHeaders.indices
                        .flatMap(itemIdEncoder.decodeItem)
                        .sorted :+ cfiTreeBuilders.head._3).toArray,
                      candidateClosedHeaders.last.frequency,
                      cfiTreeBuilders.head._1.tree
                    )) {
                  mineClosed(
                    baseItemSet,
                    minItemSetSize,
                    maxNItemSets,
                    itemIdEncoder,
                    cfiTreeBuilders,
                    candidateClosedHeaders.map(_.item),
                    candidateClosedHeaders.indices,
                    candidateClosedHeaders.last.frequency,
                    accumulator
                  )
                }
              }
            }
        } else {
          val cfiTreeBuilder = new TreeBuilder[Int, CFINode, CFIHeader](itemIdEncoder,
                                                                        (_, _, _) => new CFIHeader,
                                                                        (itemId, parent) => new CFINode(itemId, parent))

          if (cfiTreeBuilders.nonEmpty) {
            val (prevCFITreeBuilder, _, prevItemId) = cfiTreeBuilders.head

            Iterator
              .iterate(prevCFITreeBuilder.tree.headers(prevItemId).node)(_.sibling)
              .takeWhile(_ != null)
              .foreach { node =>
                val itemIdSet = Iterator
                  .iterate(node.parent)(_.parent)
                  .takeWhile(_ != null)
                  .flatMap(node => itemIdEncoder.encodeItem(node.itemId))
                  .toArray
                  .sorted

                cfiTreeBuilder.addEncoded(itemIdSet, node.frequency)
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

              if (!closedChecking(tail, header.frequency, cfiTreeBuilder.tree)) {
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
                  (cfiTreeBuilder, fpTree, itemId) +: cfiTreeBuilders,
                  accumulator
                )

                //TODO: fix prune logic, probably conditionalFPTree.headers.last.node should not be used here
                // prune = !conditionalFPTree.isEmpty && conditionalFPTree.height == itemId && conditionalFPTree.headers.last.node.frequency >= minFrequency
              }
            }
        }
      }
    }

  private def closedChecking(itemIdSet: Array[Int], frequency: Int, cfiTree: Tree[CFIHeader]): Boolean = {

    def closedChecking(itemIdSet: Array[Int], itemIdIndex: Int, node: CFINode): Boolean =
      if (itemIdIndex == -1) {
        true
      } else if (node == null) {
        false
      } else if (itemIdSet(itemIdIndex) < node.itemId) {
        closedChecking(itemIdSet, itemIdIndex, node.parent)
      } else if (itemIdSet(itemIdIndex) == node.itemId) {
        closedChecking(itemIdSet, itemIdIndex - 1, node.parent)
      } else {
        false
      }

    Iterator
      .iterate(cfiTree.headers(itemIdSet.last).node)(_.sibling)
      .takeWhile(_ != null)
      .exists(node => node.frequency >= frequency && closedChecking(itemIdSet, itemIdSet.length - 1, node))
    //TODO: optimization could be to store the nodes in frequency order, but this contradicts LeveledFPTreeHeader behaviour. Do we need LeveledFPTreeHeader in FPClose at all?
  }

  private def mineClosed(
      baseItemSet: FrequentItemSet[ItemType],
      minItemSetSize: Int,
      maxNItemSets: Int,
      itemIdEncoder: ItemEncoder[Int],
      cfiTreeBuilders: Seq[(TreeBuilder[Int, CFINode, CFIHeader], Tree[LeveledFPTreeHeader[ItemType]], Int)],
      itemSet: Seq[ItemType],
      itemIdSet: Seq[Int],
      frequency: Int,
      accumulator: FrequentItemSetAccumulator[ItemType]
  ) {
    var currentItemIdSet     = itemIdSet
    var currentItemIdEncoder = itemIdEncoder
    var currentItemSet       = itemSet
    cfiTreeBuilders.foreach {
      case (cfiTreeBuilder, conditionalFPTree, itemId) =>
        val decodedCurrentItemIdSet = currentItemIdSet.flatMap(currentItemIdEncoder.decodeItem)
        cfiTreeBuilder.addEncoded(decodedCurrentItemIdSet.toArray.sorted, frequency) // TODO: we don't add itemId to the decodedCurrentItemIdSet because it is not needed in the CFI tree,
        // the CFI tree could have 1 header less
        currentItemIdSet = itemId +: decodedCurrentItemIdSet
        currentItemIdEncoder = cfiTreeBuilder.itemEncoder
        currentItemSet +:= conditionalFPTree.headers(itemId).item
    }

    // TODO: addItems is costly, check if we really need to do it
    val frequentItemSet = baseItemSet.addItems(currentItemSet.toArray, frequency)
    if (frequentItemSet.size >= minItemSetSize && accumulator.size < maxNItemSets) {

      // DEBUG
      if (frequentItemSet.items.contains("D")) {
        println(frequentItemSet)
      }

      accumulator.add(frequentItemSet)
    }
  }

}
