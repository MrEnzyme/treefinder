package treefinder

import util.math.{Locatable, Point}

case class NodeGroup(coords: Point, nodes: Set[Int])
case class Node(name: String, coords: Point, classNode: Boolean, keystone: Boolean, effects: Map[String, Double], neighbors: Set[Int]) extends Locatable(coords)

