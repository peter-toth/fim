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

package ptoth.fim

import scala.collection.mutable.ListBuffer
import scala.io.Source

object FrequentItemSetUtils {

  def readItemSetFile(file: String): Array[Array[Int]] = {
    val buffer = ListBuffer.empty[Array[Int]]
    for (line <- Source.fromFile(file).getLines) {
      val items = line.split(' ')
      buffer += items.map(_.toInt)
    }

    buffer.toArray
  }

}
