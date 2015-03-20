package treefinder

import com.github.pathikrit.dijon
import org.apache.commons.codec.binary.Base64

import scala.collection.mutable.{LinkedHashMap, LinkedHashSet}
import scala.io.Source
import scala.util.Random
import scala.util.matching.Regex

object TreeFinder {
    def main(args: Array[String]) {
        println(exportTree(6, Seq(17788, 11334, 38807)))
        val nodes = new LinkedHashMap[Int, Node]

        val treeJson = Source.fromFile("tree.json").mkString
        //println(treeJson.substring(0, 10))
        val start = System.currentTimeMillis()
        val nodeData = dijon.parse(treeJson.substring(1)).skillTreeData.nodes.toSeq
        println("parsed json in " + (System.currentTimeMillis() - start) + " ms")

        for(node <- nodeData) {
            val neighbors = node.out.toSeq.map(_.as[Double].get.toInt).toSet
            val classNode = if(node.spc.toSeq.isEmpty) -1 else node.spc.toSeq(0).as[Double].get.toInt
            nodes(node.id.as[Double].get.toInt) = Node(node.dn.toString, node.ks.as[Boolean].get, classNode, node.sd.toSeq.map(_.toString), neighbors)
        }

        val random = randomTree(44683, nodes)
        //println(sumStats(random, nodes).map(s => s._1.replace("XX", s._2.toString)).mkString("\n"))
        //println(exportTree(6, random.toSeq))

        //val sorted = statTotals.toSeq.sorted
        //for((stat, amount) <- sorted) println(stat.replace("XXXX", amount.toString))
    }

    def sumStats(chosen: Traversable[Int], nodes: LinkedHashMap[Int, Node]) = {
        val statTotals = new LinkedHashMap[String, Double]
        val number = new Regex("\\d+(\\.\\d*)?")
        val chosenNodes = chosen.map(nodes(_))

        for(node <- chosenNodes; effect <- node.effects) {
            if(number.findAllIn(effect).length > 1) println("found 2 numbers")
            val amount = number.findFirstIn(effect)
            if(amount.isDefined) {
                val effectName = effect.replace(amount.get, "XX")
                if(statTotals.contains(effectName)) statTotals(effectName) += amount.get.toDouble
                else statTotals(effectName) = amount.get.toDouble
            }
            else statTotals(effect) = 0
        }

        statTotals
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

    def exportTree(charNum: Byte, nodes: Seq[Int]): String = {
        val bytes = new Array[Byte](nodes.size*2 + 6)
        bytes(3) = 2
        bytes(4) = charNum

        for(node <- nodes) {
            val index = nodes.indexOf(node)*2 + 6
            bytes(index) = ((node >> 8) & 0xff).toByte
            bytes(index+1) = (node & 0xff).toByte
        }

        "http://www.pathofexile.com/passive-skill-tree/" + Base64.encodeBase64String(bytes).replace("/", "_").replace("+", "-")
    }

    def randomTree(start: Int, nodes: LinkedHashMap[Int, Node]) = {
        val neighbors = determineNeighbors(nodes)
        val openSet = new LinkedHashSet[Int]
        val chosen = new LinkedHashSet[Int]

        openSet += start

        while(chosen.size < 100) {
            val next = openSet.toVector(Random.nextInt(openSet.size))
            chosen += next
            openSet -= next
            for(neighbor <- neighbors(next); if !chosen.contains(neighbor)) openSet += neighbor
        }

        chosen
    }
}