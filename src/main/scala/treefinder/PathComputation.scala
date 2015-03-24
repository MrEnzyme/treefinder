package treefinder

import java.io.{RandomAccessFile, DataOutputStream, FileInputStream, FileOutputStream}
import java.nio.channels.FileChannel

import util.PathSearch

import scala.collection.{Set, Map}

object PathComputation {
    val pathFile = "paths.dat"

    // find all shortest paths between nodes on the tree
    def computeAllPaths(pathSearch: PathSearch[Int], nodes: Set[Int], neighborNodes: Map[Int, Set[Int]]) = {
        var pathsDone = 0

        (for(node <- nodes; if neighborNodes(node).nonEmpty) yield {
            val nodePaths =
                for(otherNode <- nodes; if node != otherNode && neighborNodes(otherNode).nonEmpty) yield {
                    pathsDone += 1
                    if(pathsDone % 10000 == 0) println(pathsDone + " paths calculated")
                    otherNode -> pathSearch.getPath(node, otherNode).toIndexedSeq
                }
            node -> nodePaths.toMap
        }).toMap
    }

    def savePaths(paths: Map[Int, Map[Int, IndexedSeq[Int]]], pathFile: String) = {
        def calculateFileSize() = {
            val bytes = Integer.SIZE/8
            var size = bytes
            for((node, nodePaths) <- paths) {
                size += bytes*2
                for((otherNode, path) <- nodePaths) size += bytes*(2+path.length)
            }
            size
        }
        val channel = new RandomAccessFile(pathFile, "rw").getChannel
        val buffer = channel.map(FileChannel.MapMode.READ_WRITE, 0, calculateFileSize())
        var pathCount = 0
        buffer.putInt(paths.size)
        for((node, nodePaths) <- paths) {
            buffer.putInt(node)
            buffer.putInt(nodePaths.size)
            for((otherNode, path) <- nodePaths) {
                buffer.putInt(otherNode)
                buffer.putInt(path.length)
                pathCount += 1
                if(pathCount % 10000 == 0) println(pathCount + " paths saved")
                for(p <- path) buffer.putInt(p)
            }
        }
        channel.close()
    }

    def loadPaths(pathFile: String): Map[Int, Map[Int, IndexedSeq[Int]]] = {
        val startTime = System.currentTimeMillis()

        val channel = new FileInputStream(pathFile).getChannel
        val buffer = channel.map(FileChannel.MapMode.READ_ONLY, 0, channel.size())
        val nodeCount = buffer.getInt
        val paths = for(i <- 0 until nodeCount) yield {
            val nodeId = buffer.getInt
            val pathCount = buffer.getInt
            val nodePaths = for(j <- 0 until pathCount) yield {
                val otherNode = buffer.getInt
                val pathLength = buffer.getInt
                val path = for(k <- 0 until pathLength) yield buffer.getInt
                otherNode -> path
            }
            nodeId -> nodePaths.toMap
        }
        channel.close()
        println("took " + (System.currentTimeMillis() - startTime) + "ms to read file")
        paths.toMap
    }
}
