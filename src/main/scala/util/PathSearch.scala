package util

import scala.collection.{Map, Set}
import scala.collection.mutable.{HashMap, HashSet,PriorityQueue}

class PathSearch[A](getNeighbors: A => Set[A], getDistance: (A, A) => Double, estimateDistance: (A, A) => Double) {
    val fScores = new HashMap[A, Double]
    val gScores = new HashMap[A, Double]

    val openPQ = new PriorityQueue[A]()(new Ordering[A] {
        def compare(a: A, b: A) = {
            val result = fScores(b) - fScores(a)
            if(result < 0.0) -1
            else if(result > 0.0) 1
            else 0
        }
    })

    val openSet = new HashSet[A]
    val closedSet = new HashSet[A]

    val cameFrom = new HashMap[A, A]

    def resetLists() {
        gScores.clear()
        fScores.clear()
        openPQ.clear()
        openSet.clear()
        closedSet.clear()
        cameFrom.clear()
    }

    def getPath(startNode: A, destNode: A): List[A] = {
        resetLists()

        gScores.put(startNode, 0)
        fScores.put(startNode, estimateDistance(startNode, destNode))
        openPQ += startNode
        openSet += startNode
        while(openSet.nonEmpty) {
            val current = openPQ.dequeue()
            if(current == destNode) return reconstructPath(List(current), current)

            openSet -= current
            closedSet += current
            for(n <- getNeighbors(current)) {
                val tentGScore = gScores(current) + getDistance(current, n)
                if(closedSet.contains(n) && tentGScore >= gScores(n)) {}
                else if(!openSet.contains(n) || tentGScore < gScores(n)) {
                    cameFrom(n) = current
                    gScores(n) = tentGScore
                    fScores(n) = gScores(n) + estimateDistance(n, destNode)
                    if(!openSet.contains(n)) {
                        openPQ += n
                        openSet += n
                    }
                }
            }
        }
        null
    }

    def reconstructPath(path: List[A], dest: A): List[A] = {
        if(!cameFrom.contains(dest)) return path
        reconstructPath(cameFrom(dest) :: path, cameFrom(dest))
    }
}
