package util.math


abstract class Locatable(val pos: Point) {
    def x = pos.x
    def y = pos.y
    def distanceTo(l: Locatable) = pos.distanceTo(l.pos)
}
