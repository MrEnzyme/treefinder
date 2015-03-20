package treefinder

case class Node(name: String, keystone: Boolean, classNode: Int, effects: Traversable[String], neighbors: Set[Int])
