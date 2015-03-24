package treefinder

import util.PathSearch

import scala.collection.{Map, Set}
import scala.collection.mutable.{LinkedHashSet, LinkedHashMap}

class TreeSearch(nodeSet: Map[Int, Node]) {
    // map of neighbor nodes
    val neighbors = determineNeighbors(nodeSet)

    // total amount of each stat occurence on the tree
    val treeStatTotals = sumStats(nodeSet.keys).toMap

    // average numerical values for a given node of each stat
    val averageStatValues: Map[String, Double] = for((stat, amount) <- treeStatTotals) yield (stat, amount/nodeSet.values.count(_.effects.contains(stat)))

    val pathSearch = new PathSearch[Int](n => neighbors(n), (a, b) => 1, (a, b) => 1.0 - 1.0/nodeSet(a).distanceTo(nodeSet(b)))

    def findTree(constraints: ConstraintSet, paths: Map[Int, Map[Int, IndexedSeq[Int]]]): Set[Int] = {
        val tree = new LinkedHashSet[Int]
        val openSet = new LinkedHashSet[Int]

        val requiredNodes = constraints.keystones
        val optionalNodes = Set()
        val relevantNodes = requiredNodes ++ optionalNodes

        tree += 44683
        openSet ++= List(45272, 17788)

        def getPath(a: Int, b: Int) = paths(a)(b)
        def getDistance(a: Int, b: Int) = getPath(a, b).length

        def distanceToTree(node: Int, tree: Set[Int]) = getDistance(node, tree.minBy(getDistance(node, _)))

        // scores a node in isolation based on its point-value
        def scoreNode(node: Int): Double = {
            if(requiredNodes.contains(node)) return 1.0
            // take only the effects in this node that we care about
            val relevantEffects = nodeSet(node).effects.filterKeys(constraints.effects.contains)
            // map their values relative to the average and sum them to get the final score
            relevantEffects.map(e => e._2/averageStatValues(e._1)).sum
        }

        // score all the nodes in the tree
        val nodeScores: Map[Int, Double] = for((id, node) <- nodeSet) yield id -> scoreNode(id)

        // finds the overall score for a node, accounting for its distance to other nodes relative to their point-value
        def evaluateNode(node: Int, relevant: Set[Int]): Double = {
            if(requiredNodes.contains(node)) return nodeScores(node)
            val sum = relevant.toSeq.map(n => nodeScores(n)*getDistance(node, n))
            if(node == 45272 || node == 17788) println(relevant.size, node, sum.sum, sum)
            nodeScores(node) + relevant.toSeq.map(n => nodeScores(n)*getDistance(node, n)).sum
        }

        while(!satisfiesConstraints(tree, constraints)) {
            val remainingRelevantNodes = relevantNodes.diff(tree)
            val nextNode = openSet.minBy(evaluateNode(_, remainingRelevantNodes))
            for(node <- openSet) print(node, nodeSet(node).name, nodeSet(node).coords, evaluateNode(node, remainingRelevantNodes))
            println("\nchoosing ", nodeSet(nextNode).name, nextNode)
            openSet -= nextNode
            tree += nextNode
            openSet ++= neighbors(nextNode).diff(tree)
            println("tree size:", tree.size)
        }

        tree - 44683
    }

    // check if a given tree satisfies a set of constraints
    def satisfiesConstraints(tree: Set[Int], constraints: ConstraintSet): Boolean = {
        if(tree.size > constraints.maxPoints) return false

        // ensure all the required keystones are present
        for(keystone <- constraints.keystones; if !tree.contains(keystone)) return false

        // ensure the tree's stat totals are within bounds for each given effect
        val statTotals = sumStats(tree)
        for((name, effect) <- constraints.effects)
            if(effect.max < statTotals(name) || effect.min > statTotals(name)) return false

        true
    }

    // returns a map of nodes to the set of their neighbors
    def determineNeighbors(nodes: Map[Int, Node]): Map[Int, Set[Int]] = {
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
    def sumStats(tree: Traversable[Int]): Map[String, Double] = {
        val statTotals = new LinkedHashMap[String, Double]

        val chosenNodes = tree.map(nodeSet(_))

        for(node <- chosenNodes; (effect, amount) <- node.effects; if !node.keystone) {
            if(statTotals.contains(effect)) statTotals(effect) += amount
            else statTotals(effect) = amount
        }

        statTotals
    }
}
