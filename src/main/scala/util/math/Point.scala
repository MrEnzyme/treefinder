package util.math

case class Point(x: Double, y: Double) {
    def distanceTo(p: Point) = math.sqrt(math.pow(p.x - x,2) + math.pow(p.y - y, 2))
    def +(p: Point) = Point(x + p.x, y + p.y)
    def -(p: Point) = Point(x - p.x, y - p.y)
}