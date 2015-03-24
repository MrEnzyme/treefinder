package treefinder

import com.github.pathikrit.dijon
import org.apache.commons.codec.binary.Base64
import util.PathSearch
import util.math.Point

import scala.collection.mutable.LinkedHashMap
import scala.io.Source
import scala.util.matching.Regex
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object TreeFinder {
    val treeFile = "tree.json"
    val pathFile = "paths.dat"

    def main(args: Array[String]) {
        // load the tree data and path map concurrently
        val loadResources = for {
            nodes <- Future(loadTree(treeFile))
            paths <- Future(PathComputation.loadPaths(pathFile))
        } yield (nodes, paths)

        val (nodes, paths) = Await.result(loadResources, 15.seconds)
        println(paths(17788)(11455))
        println(nodes.keys.max)
        val search = new TreeSearch(nodes)

    }

    def generatePathsFile() {
        val nodes = loadTree(treeFile)
        val search = new TreeSearch(nodes).pathSearch
        val paths = PathComputation.computeAllPaths(search, search.nodes, search.neighborNodes)
        PathComputation.savePaths(paths, pathFile)
    }

    // load the skill tree nodes into a map by id
    def loadTree(fileName: String) = {
        val nodes = new LinkedHashMap[Int, Node]

        val treeJson = Source.fromFile(fileName).mkString
        val start = System.currentTimeMillis()
        val tree = dijon.parse(treeJson.substring(treeJson.indexOf{"{"})).skillTreeData
        println("parsed json in " + (System.currentTimeMillis() - start) + " ms")

        val groups = for((id, group) <- tree.groups.toMap)
        yield (id.toInt, NodeGroup(Point(group.x.as[Double].get, group.y.as[Double].get), group.n.toSeq.map(_.as[Double].get.toInt).toSet))

        val numberPattern = new Regex("\\d+(\\.\\d*)?")

        // find the seven invisible root class nodes so they can be removed from the search tree
        val classNodes = tree.nodes.toSeq.filter(_.spc.toSeq.nonEmpty).map(_.id.as[Double].get.toInt).toSet

        // constants for calculating node positions relative to group
        val skillsPerOrbit = Array[Int](1, 6, 12, 12, 12)
        val orbitRadii = Array[Float](0, 81.5f, 163, 326, 489)

        for(node <- tree.nodes.toSeq) {
            val neighbors = node.out.toSeq.map(_.as[Double].get.toInt).toSet

            // offset the node position from its group position using orbit/orbitIndex
            val orbit = node.o.as[Double].get.toInt
            val orbitIndex = node.oidx.as[Double].get
            val d = orbitRadii(orbit)
            val b = (2*Math.PI*orbitIndex)/skillsPerOrbit(orbit)
            val coords = groups(node.g.as[Double].get.toInt).coords - Point(d*math.sin(-b), d*math.cos(-b))

            val effects =
                for(effect <- node.sd.toSeq.map(_.toString)) yield {
                    val amount = numberPattern.findFirstIn(effect)
                    if(amount.isDefined) (effect.replace(amount.get, "XX"), amount.get.toDouble)
                    else (effect, 1.0)
                }
            nodes(node.id.as[Double].get.toInt) = Node(node.dn.toString, coords, node.ks.as[Boolean].get, effects.toMap, neighbors.diff(classNodes))
        }

        nodes
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
}