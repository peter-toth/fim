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

// scalastyle:off null
class BuilderNode[NodeType >: Null <: Node[NodeType]](val node: NodeType = null) {
// scalastyle:on

  lazy val children: mutable.Map[Int, BuilderNode[NodeType]] = mutable.Map.empty

  def add(itemIdSet: Array[Int],
          itemIdIndex: Int,
          headers: Array[_ <: Header[NodeType]],
          data: NodeType#DataType,
          nodeCreator: (Int, NodeType) => NodeType): (Int, Boolean) =
    if (itemIdIndex == itemIdSet.size) {
      (0, false)
    } else {
      val itemId        = itemIdSet(itemIdIndex)
      var sizeIncrement = 0
      var split         = false
      val child = children.getOrElseUpdate(
        itemId, {
          val node = nodeCreator(itemId, this.node)
          headers(itemId).prepend(node)

          sizeIncrement = 1
          split = children.size >= 1

          new BuilderNode(node)
        }
      )
      child.node.update(data)
      val (childSizeIncrement, childSplit) = child.add(itemIdSet, itemIdIndex + 1, headers, data, nodeCreator)

      (sizeIncrement + childSizeIncrement, split || childSplit)
    }

}

class TreeBuilder[ItemType, NodeType >: Null <: Node[NodeType], HeaderType <: Header[NodeType]: ClassTag](
    itemEncoder: ItemEncoder[ItemType],
    headerCreator: (Int, ItemType, Int) => HeaderType,
    nodeCreator: (Int, NodeType) => NodeType
) {
  val tree = new Tree[HeaderType](itemEncoder.itemFrequencies.zipWithIndex.map {
    case ((item, frequency), itemId) => headerCreator(itemId, item, frequency)
  })

  private val builderNode = new BuilderNode[NodeType]()

  def addEncoded(itemIdSet: Array[Int], data: NodeType#DataType): TreeBuilder[ItemType, NodeType, HeaderType] = {
    val (nNodes, split) = builderNode.add(itemIdSet, 0, tree.headers, data, nodeCreator)
    tree.nNodes += nNodes
    tree.singlePath = tree.singlePath && !split
    tree.nItemSets += 1

    this
  }

  def add(itemset: Array[ItemType], data: NodeType#DataType): TreeBuilder[ItemType, NodeType, HeaderType] = {
    val itemIdSet: Array[Int] = itemEncoder.encodeItems(itemset)

    addEncoded(itemIdSet, data)
  }

}
