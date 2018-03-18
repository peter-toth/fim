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

import scala.collection.mutable

class Node(val parent: Node, val itemId: Int) {

  var frequency: Int = 0
  // scalastyle:off null
  var sibling: Node = null
  // scalastyle:on

  def path: String = s"${if (parent == null) "null" else parent.path}->$itemId"

  override def toString: String = s"Node($path: $frequency)"

}

class Header[ItemType](val item: ItemType, val frequency: Int) {

  // scalastyle:off null
  var node: Node = null
  // scalastyle:on

  def prepend(node: Node): Unit = {
    node.sibling = this.node
    this.node = node
  }

}

class FPTree[ItemType](val headers: Array[Header[ItemType]]) {
  var nNodes: Int    = 0
  var nItemSets: Int = 0

  override def toString: String =
    s"Header(\n${headers.zipWithIndex
      .map { case (header, itemId) => s"  $itemId - ${header.item} - ${header.frequency} - ${header.node}" }
      .mkString("\n")}\n)"

}

object FPTree {

  def apply[ItemType](itemsets: Array[Array[ItemType]], minFrequency: Int): FPTree[ItemType] = {
    val itemFrequencies = mutable.Map.empty[ItemType, Int]
    itemsets.foreach(_.toSet[ItemType].foreach(item => itemFrequencies(item) = itemFrequencies.getOrElse(item, 0) + 1))

    val fpTreeBuilder = FPTreeBuilder(itemFrequencies.toMap, minFrequency)

    itemsets.foreach(fpTreeBuilder.add(_))

    fpTreeBuilder.fpTree
  }

  def apply[ItemType](baseFPTree: FPTree[ItemType], baseItemId: Int, minFrequency: Int): FPTree[ItemType] = {
    val oldItemIdAndFrequencies = mutable.Map.empty[Int, Int];

    var node = baseFPTree.headers(baseItemId).node;
    while (node != null) {
      var currentNode = node.parent
      while (currentNode != null) {
        oldItemIdAndFrequencies(currentNode.itemId) = oldItemIdAndFrequencies.getOrElse(currentNode.itemId, 0) + node.frequency

        currentNode = currentNode.parent;
      }

      node = node.sibling;
    }

    val oldItemIdToNewItemIdMap = mutable.Map.empty[Int, Int]
    val headers = oldItemIdAndFrequencies
      .filter(_._2 >= minFrequency)
      .toArray
      .sortBy(-_._2)
      .zipWithIndex
      .map {
        case ((oldItemId, frequency), index) =>
          oldItemIdToNewItemIdMap(oldItemId) = index
          new Header(baseFPTree.headers(oldItemId).item, frequency)
      }

    val fpTree = new FPTree(headers)

    val builderNode = new FPBuilderNode[ItemType]()
    node = baseFPTree.headers(baseItemId).node;
    while (node != null) {
      var currentNode  = node.parent;
      val newItemIdSet = mutable.ListBuffer.empty[Int];
      while (currentNode != null) {
        oldItemIdToNewItemIdMap.get(currentNode.itemId).foreach(newItemId => newItemIdSet += newItemId);

        currentNode = currentNode.parent;
      }

      fpTree.nNodes += builderNode.add(newItemIdSet.toArray.sorted, 0, headers, node.frequency)
      fpTree.nItemSets += node.frequency;

      node = node.sibling;
    }

    fpTree
  }

}

class FPBuilderNode[ItemType](val node: Node = null) {

  lazy val children: mutable.Map[Int, FPBuilderNode[ItemType]] = mutable.Map.empty

  def add(itemIdSet: Array[Int], itemIdIndex: Int, headers: Array[Header[ItemType]], frequency: Int): Int =
    if (itemIdIndex < itemIdSet.size) {
      val itemId = itemIdSet(itemIdIndex)

      var sizeIncrement = 0
      val child = children.getOrElseUpdate(
        itemId, {
          val node = new Node(this.node, itemId)
          headers(itemId).prepend(node)

          sizeIncrement = 1

          new FPBuilderNode(node)
        }
      )

      child.node.frequency += frequency

      child.add(itemIdSet, itemIdIndex + 1, headers, frequency)

      sizeIncrement
    } else {
      0
    }

}

class FPTreeBuilder[ItemType](val fpTree: FPTree[ItemType]) {
  val itemToItemIdMap = mutable.Map.empty[ItemType, Int]
  var itemId          = 0
  while (itemId < fpTree.headers.size) {
    itemToItemIdMap(fpTree.headers(itemId).item) = itemId
    itemId += 1
  }

  val builderNode = new FPBuilderNode[ItemType]()

  def add(itemset: Array[ItemType]): FPTreeBuilder[ItemType] = {
    val itemIdSet = itemset.toSet[ItemType].collect(Function.unlift(item => itemToItemIdMap.get(item))).toArray

    fpTree.nNodes += builderNode.add(itemIdSet.sorted, 0, fpTree.headers, 1)
    fpTree.nItemSets += 1

    this
  }

}

object FPTreeBuilder {

  def apply[ItemType](itemFrequencies: Map[ItemType, Int], minFrequency: Int): FPTreeBuilder[ItemType] = {
    val headers = itemFrequencies
      .filter(_._2 >= minFrequency)
      .toArray
      .sortBy(-_._2)
      .map { case (item, frequency) => new Header(item, frequency) }

    val fpTree = new FPTree(headers)
    new FPTreeBuilder[ItemType](fpTree)
  }

}
