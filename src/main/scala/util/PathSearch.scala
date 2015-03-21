package util

import treefinder.Node

import scala.collection.mutable.{HashMap, HashSet, LinkedHashMap, LinkedHashSet, PriorityQueue}

class PathSearch(nodes: LinkedHashMap[Int, Node], neighborNodes: LinkedHashMap[Int, LinkedHashSet[Int]]) {
    val fScores = new HashMap[Int, Double]
    val gScores = new HashMap[Int, Double]

    val pathCache = new HashMap[Tuple2[Int, Int], List[Int]]

    val openPQ = new PriorityQueue[Int]()(new Ordering[Int]
    {
        def compare(a: Int, b: Int) = (fScores(b) - fScores(a)).toInt
    })

    val openSet = new HashSet[Int]
    val closedSet = new HashSet[Int]

    val cameFrom = new HashMap[Int, Int]

    def resetLists() {
        gScores.clear()
        fScores.clear()
        openPQ.clear()
        openSet.clear()
        closedSet.clear()
        cameFrom.clear()
    }

    def getPath(startNode: Int, destNode: Int): List[Int] =
    {
        def distance(a: Int, b: Int) = nodes(a).distanceTo(nodes(b))
        //if we already searched this path, return the cached path
        if(pathCache.contains((startNode, destNode))) return pathCache((startNode, destNode))

        resetLists()

        gScores.put(startNode, 0)
        fScores.put(startNode, distance(startNode, destNode))
        openPQ += startNode
        openSet += startNode
        while(openSet.nonEmpty)
        {
            val current = openPQ.dequeue()
            if(current == destNode)
            {
                val p = reconstructPath(List(current), current)
                //dont cache the shortest paths, there will be tons of them and they are cheap to compute
                if(p.size > 3) pathCache.put((startNode, destNode), p)
                return p
            }
            openSet -= current
            closedSet += current
            for(n <- neighborNodes(current))
            {
                val tentGScore = gScores(current) + distance(current, n)
                if(closedSet.contains(n) && tentGScore >= gScores(n)) {}
                else if(!openSet.contains(n) || tentGScore < gScores(n))
                {
                    cameFrom(n) = current
                    gScores(n) = tentGScore
                    fScores(n) = gScores(n) + distance(n, destNode)
                    if(!openSet.contains(n))
                    {
                        openPQ += n
                        openSet += n
                    }
                }
            }
        }
        null
    }

    def reconstructPath(path: List[Int], dest: Int): List[Int] =
    {
        if(!cameFrom.contains(dest)) return path
        reconstructPath(cameFrom(dest) :: path, cameFrom(dest))
    }
}
