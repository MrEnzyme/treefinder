package treefinder

import util.PathSearch

import scala.collection.{Map, Set}
import scala.collection.mutable.{HashMap, LinkedHashMap, HashSet, PriorityQueue, LinkedHashSet}

class TreeSearch(nodeSet: Map[Int, Node]) {
    // map of neighbor nodes
    val neighbors = determineNeighbors(nodeSet)

    // total amount of each stat occurence on the tree
    val treeStatTotals = sumStats(nodeSet.keys).toMap

    // average numerical values for a given node of each stat
    val averageStatValues: Map[String, Double] = for((stat, amount) <- treeStatTotals) yield (stat, amount/nodeSet.values.count(_.effects.contains(stat)))

    val pathSearch = new PathSearch[Int](n => neighbors(n), (a, b) => 1, (a, b) => 1.0 - 1.0/nodeSet(a).distanceTo(nodeSet(b)))

    val debug = true

    def findTree(constraints: ConstraintSet, paths: Map[Int, Map[Int, IndexedSeq[Int]]]): Set[Int] = {
        val tree = Set(44683)
        val openSet = Set(45272, 17788)

        val requiredNodes = constraints.keystones
        val optionalNodes: Set[Int] = Set()
        val relevantNodes = requiredNodes ++ optionalNodes

        def getPath(a: Int, b: Int) = if(a == b) Seq(a) else paths(a)(b)
        def getDistance(a: Int, b: Int) = getPath(a, b).length

        def distanceToTree(node: Int, tree: Set[Int]) = getDistance(node, tree.minBy(getDistance(node, _)))
        def getPathToTree(node: Int, tree: Set[Int]) = getPath(node, tree.minBy(getDistance(node, _)))

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
        def evaluateNode(node: Int, tree: Set[Int], relevant: Set[Int]): Double = {
            if(requiredNodes.contains(node)) return 0.0

            val newTree = tree + node
            val overlaidPathsToTree = relevant.map(getPathToTree(_, newTree)).flatten.diff(newTree)

            def evaluateNodeInPaths(n: Int) = {
                relevant.map(getPathToTree(_, newTree + n)).flatten.diff(newTree).size + distanceToTree(n, newTree)
            }
            val bestNodeInPaths = overlaidPathsToTree.minBy(evaluateNodeInPaths)
            if(debug) {
                println("best node for " + nodeSet(node).name + ": " + (evaluateNodeInPaths(bestNodeInPaths), nodeSet(bestNodeInPaths).name) + ", " + TreeFinder.exportTree(6, Seq(bestNodeInPaths)))
                val paths = relevant.map(getPathToTree(_, newTree + bestNodeInPaths)).flatten.diff(newTree).toSeq
                println("paths to best node, size " + paths.size + ": " + TreeFinder.exportTree(6, paths))
            }

            evaluateNodeInPaths(bestNodeInPaths) - nodeScores(node)
        }

        def search(tree: Set[Int], openSet: Set[Int]): Set[Int] = {
            if(satisfiesConstraints(tree, constraints)) return tree

            val remainingRelevantNodes = relevantNodes.diff(tree)

            val sortedNodes = openSet.toSeq.sortBy(evaluateNode(_, tree, remainingRelevantNodes))
            val bestNode = sortedNodes.head

            if(debug) {
                println("========\n" + TreeFinder.exportTree(6, tree.toSeq))
                for(n <- sortedNodes) {
                    if (!remainingRelevantNodes.contains(n)) {
                        val paths = remainingRelevantNodes.map(getPathToTree(_, tree + n)).flatten.diff(tree + n)
                        val treeUrl = TreeFinder.exportTree(6, paths.toSeq)
                        println(paths.size, treeUrl)
                        if (treeUrl == "http://www.pathofexile.com/passive-skill-tree/AAAAAgYARXwPq66Lsw4hwDpCBbXdqC1HbWxQQjBxsNjndDy9S3gc3CxG1CPo1g==") {

                        }
                    }
                    println(nodeSet(n).name, n, evaluateNode(n, tree, remainingRelevantNodes))
                }
                println()
            }

            search(tree + bestNode, openSet ++ neighbors(bestNode).diff(tree) - bestNode)
        }

        search(tree, openSet) - 44683
    }

    def findTreeAStar(constraints: ConstraintSet, paths: Map[Int, Map[Int, IndexedSeq[Int]]]): Set[Int] = {
        type Tree = Set[Int]
        val fScores = new HashMap[Tree, Double]
        val gScores = new HashMap[Tree, Double]

        val openPQ = new PriorityQueue[Tree]()(new Ordering[Tree] {
            def compare(a: Tree, b: Tree) = (fScores(b) - fScores(a)).toInt
        })

        val openSet = new HashSet[Tree]
        val closedSet = new HashSet[Tree]

        val cameFrom = new HashMap[Tree, Tree]

        val startNode = Set(44683)

        val requiredNodes = constraints.keystones
        val optionalNodes: Set[Int] = Set()
        val relevantNodes = requiredNodes ++ optionalNodes

        def getPath(a: Int, b: Int) = if(a == b) Seq(a) else paths(a)(b)
        def getDistance(a: Int, b: Int) = getPath(a, b).length

        def distanceToTree(node: Int, tree: Set[Int]) = getDistance(node, tree.minBy(getDistance(node, _)))
        def getPathToTree(node: Int, tree: Set[Int]) = getPath(node, tree.minBy(getDistance(node, _)))

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

        def estimateRemainingPoints(tree: Tree): Double = {
            val relevant = relevantNodes.diff(tree)

            def getMinSpanningTree: Set[Int] = {
                var vertices = Set(relevant.head)
                val edges = new LinkedHashSet[Int]

                while(vertices != relevant) {
                    val remaining = relevant.diff(vertices)
                    val nearest = remaining.minBy(distanceToTree(_, vertices))
                    edges ++= getPathToTree(nearest, vertices)
                    vertices += nearest
                }

                edges
            }
            val mst = getMinSpanningTree

            val combined = mst ++ getPathToTree(mst.minBy(distanceToTree(_, tree)), tree)
            println(TreeFinder.exportTree(6, combined.toSeq))
            combined.size
        }

        // get all the neighbor trees
        def getNeighbors(tree: Tree): Set[Tree] = tree.map(neighbors(_)).flatten.map(tree + _)

        gScores(startNode) = 0
        fScores(startNode) = estimateRemainingPoints(startNode)
        openPQ += startNode
        openSet += startNode
        var searchCount = 0
        while(openSet.nonEmpty) {
            searchCount += 1
            if(searchCount % 100 == 0) println("searched " + searchCount)
            val current = openPQ.dequeue()
            if(satisfiesConstraints(current, constraints)) return current - 44683

            openSet -= current
            closedSet += current
            for(n <- getNeighbors(current)) {
                val tentGScore = gScores(current) + 1
                if(closedSet.contains(n) && tentGScore >= gScores(n)) {}
                else if(!openSet.contains(n) || tentGScore < gScores(n)) {
                    cameFrom(n) = current
                    gScores(n) = tentGScore
                    fScores(n) = gScores(n) + estimateRemainingPoints(n)
                    if(!openSet.contains(n)) {
                        openPQ += n
                        openSet += n
                    }
                }
            }
        }
        null
    }

    // check if a given tree satisfies a set of constraints
    def satisfiesConstraints(tree: Set[Int], constraints: ConstraintSet): Boolean = {
        // ensure all the required keystones are present
        if(!constraints.keystones.subsetOf(tree)) return false

        if(tree.size > constraints.maxPoints) return false

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
