package treefinder

import util.PathSearch

import scala.collection.mutable.{LinkedHashSet, LinkedHashMap}

class TreeSearch(nodeSet: LinkedHashMap[Int, Node]) {
    // map of neighbor nodes
    val neighbors = determineNeighbors(nodeSet)

    // total amount of each stat occurence on the tree
    val treeStatTotals = sumStats(nodeSet.keys).toMap

    // average numerical values for a given node of each stat
    val averageStatValues: Map[String, Double] = for((stat, amount) <- treeStatTotals) yield (stat, amount/nodeSet.values.count(_.effects.contains(stat)))

    val pathSearch = new PathSearch(nodeSet, neighbors)

    def findTree(constraints: ConstraintSet) = {
        val openSet = new LinkedHashSet[Int]
        val tree = new LinkedHashSet[Int]

        val requiredNodes = constraints.keystones

        openSet ++= List(17788, 45272)

        while(!satisfiesConstraints(tree, constraints)) {

        }
    }

    def scoreTree(tree: LinkedHashSet[Int], constraints: ConstraintSet): Double = {
        def scoreKeystones = tree.intersect(constraints.keystones).size
        def scoreEffects = for((stat, amount) <- sumStats(tree); if constraints.effectNames.contains(stat)) yield amount/averageStatValues(stat)

        scoreKeystones + scoreEffects.sum
    }

    // check if a given tree satisfies a set of constraints
    def satisfiesConstraints(tree: LinkedHashSet[Int], constraints: ConstraintSet): Boolean = {
        if(tree.size > constraints.maxPoints) return false

        // ensure all the required keystones are present
        for(keystone <- constraints.keystones; if !tree.contains(keystone)) return false

        // ensure the tree's stat totals are within bounds for each given effect
        val statTotals = sumStats(tree)
        for(effect <- constraints.effects)
            if(effect.max < statTotals(effect.name) || effect.min > statTotals(effect.name)) return false

        true
    }

    // returns a map of nodes to the set of their neighbors
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

    // returns a map of total values for each stat in the given tree
    def sumStats(tree: Traversable[Int]) = {
        val statTotals = new LinkedHashMap[String, Double]

        val chosenNodes = tree.map(nodeSet(_))

        for(node <- chosenNodes; (effect, amount) <- node.effects; if !node.keystone) {
            if(statTotals.contains(effect)) statTotals(effect) += amount
            else statTotals(effect) = amount
        }

        statTotals
    }
}
