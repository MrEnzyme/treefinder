package treefinder

case class Effect(name: String, max: Double = Double.MaxValue, min: Double = Double.MinValue)
case class ConstraintSet(maxPoints: Int = 120, effects: Set[Effect] = Set(), keystones: Set[Int] = Set()) {
    val effectNames = effects.map(_.name)
}
