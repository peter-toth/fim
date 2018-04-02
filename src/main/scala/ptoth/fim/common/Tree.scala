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

abstract class Node[NodeType >: Null <: Node[NodeType]](val itemId: Int, val parent: NodeType) {

  type DataType
  def update(data: NodeType#DataType)

  // scalastyle:off null
  var sibling: NodeType = null
  // scalastyle:on

  def path: String = s"${if (parent == null) "null" else parent.path}->$itemId"

  override def toString: String = s"Node($path)"

}

class Header[NodeType >: Null <: Node[NodeType]] {

  // scalastyle:off null
  var node: NodeType = null
  // scalastyle:on

  def prepend(node: NodeType): Unit = {
    node.sibling = this.node
    this.node = node
  }

}

class Tree[HeaderType <: Header[_]](val headers: Array[HeaderType]) {

  var nNodes: Int         = 0
  var singlePath: Boolean = true
  var nItemSets: Int      = 0

  def isEmpty: Boolean = nNodes == 0

  override def toString: String =
    s"Header(\n${headers.zipWithIndex
      .map {
        case (header, itemId) =>
          s"  $itemId - ${header}"
      }
      .mkString("\n")}\n)"

}
