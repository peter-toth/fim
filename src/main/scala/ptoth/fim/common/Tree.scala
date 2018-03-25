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

package ptoth.fim.common

import scala.collection.mutable
import scala.reflect.ClassTag

class Node(val parent: Node, val itemId: Int) {

  var frequency: Int = 0
  // scalastyle:off null
  var sibling: Node = null
  // scalastyle:on

  def path: String = s"${if (parent == null) "null" else parent.path}->$itemId"

  override def toString: String = s"Node($path: $frequency)"

}

class Header {

  // scalastyle:off null
  var node: Node = null
  // scalastyle:on

  def prepend(node: Node): Unit = {
    node.sibling = this.node
    this.node = node
  }

}

class Tree[HeaderType <: Header](val headers: Array[HeaderType]) {

  var nNodes: Int    = 0
  var nItemSets: Int = 0

  override def toString: String =
    s"Header(\n${headers.zipWithIndex
      .map {
        case (header, itemId) =>
          s"  $itemId - ${header}"
      }
      .mkString("\n")}\n)"

}

//object Tree {
//
//  def apply[ItemType](itemsets: Array[Array[ItemType]], minFrequency: Int, itemEncoder: ItemEncoder[ItemType]): FPTree = {
//    val itemFrequencies = mutable.Map.empty[ItemType, Int]
//    itemsets.foreach(_.toSet[ItemType].foreach(item => itemFrequencies(item) = itemFrequencies.getOrElse(item, 0) + 1))
//
//    val fpTreeBuilder = FPTreeBuilder(itemFrequencies.toMap, minFrequency)
//
//    itemsets.foreach(fpTreeBuilder.add(_))
//
//    fpTreeBuilder.fpTree
//  }
//
//}

class BuilderNode(val node: Node = null) {

  lazy val children: mutable.Map[Int, BuilderNode] = mutable.Map.empty

  def add(itemIdSet: Array[Int], itemIdIndex: Int, headers: Array[_ <: Header], frequency: Int): Int =
    if (itemIdIndex < itemIdSet.size) {
      val itemId        = itemIdSet(itemIdIndex)
      var sizeIncrement = 0
      val child = children.getOrElseUpdate(
        itemId, {
          val node = new Node(this.node, itemId)
          headers(itemId).prepend(node)

          sizeIncrement = 1

          new BuilderNode(node)
        }
      )
      child.node.frequency += frequency
      child.add(itemIdSet, itemIdIndex + 1, headers, frequency)

      sizeIncrement
    } else {
      0
    }

}

class TreeBuilder[ItemType, HeaderType <: Header](
    itemEncoder: ItemEncoder[ItemType],
    headerCreator: (Int, ItemType, Int) => HeaderType
)(implicit ev: ClassTag[HeaderType]) {

  val fpTree: Tree[HeaderType] = new Tree(itemEncoder.itemFrequencies.zipWithIndex.map {
    case ((item, frequency), itemId) => headerCreator(itemId, item, frequency)
  }.toArray)

  private val builderNode = new BuilderNode()

  def add(itemset: Array[ItemType], frequency: Int): TreeBuilder[ItemType, HeaderType] = {
    val itemIdSet = itemEncoder.encodeItems(itemset)

    fpTree.nNodes += builderNode.add(itemIdSet, 0, fpTree.headers, frequency)
    fpTree.nItemSets += 1

    this
  }

}

//object TreeBuilder {
//
//  def apply[ItemType, HeaderType <: Header](
//      itemEncoder: ItemEncoder[ItemType],
//      headerGenerator: (ItemType, Int, Int) => HeaderType
//  ): TreeBuilder[ItemType, HeaderType] = new TreeBuilder(itemEncoder, headerGenerator)
//
//}
