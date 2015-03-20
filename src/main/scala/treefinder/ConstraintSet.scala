package treefinder

case class Constraint(name: String, max: Double = Double.MaxValue, min: Double = Double.MinValue)
case class ConstraintSet(minPoints: Int = 0,
                         maxPoints: Int = 120,
                         constraints: Traversable[Constraint] = Set(),
                         keystones: Traversable[String] = Set())
