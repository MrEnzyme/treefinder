package treefinder

import scala.collection.mutable.{LinkedHashSet, LinkedHashMap}

class TreeSearch(nodeSet: LinkedHashMap[Int, Node]) {
    val neighbors = determineNeighbors(nodeSet)

    def findTree(constraints: ConstraintSet) = {

    }

    def determineNeighbors(nodes: LinkedHashMap[Int, Node]) = {
        val neighborMap = new LinkedHashMap[Int, LinkedHashSet[Int]]
        // make the first pass through
        for((id, node) <- nodes) {
            if(!neighborMap.contains(id)) neighborMap(id) = new LinkedHashSet[Int]
            neighborMap(id) ++= node.neighbors
        }
        for((id, neighbors) <- neighborMap; neighbor <- neighbors) neighborMap(neighbor) += id
        neighborMap
    }
}
