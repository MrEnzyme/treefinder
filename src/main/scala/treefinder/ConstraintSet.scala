package treefinder

import scala.collection.{Map, Set}

case class Effect(max: Double = Double.MaxValue, min: Double = 0.0)
case class ConstraintSet(maxPoints: Int = 120, effects: Map[String, Effect] = Map(), keystones: Set[Int] = Set())
