/**
 * Represents an (x, y) point on a plane.
 */
type Point = (f64, f64);

/**
 * Returns a reference to the point whose x position is greatest.
 * If the two points have equal x positions, it should return p1.
 */
pub fn rightmost_point(p1: Point, p2: Point) -> Point {
    let (p1x, p1y) = p1;
    let (p2x, p2y) = p2;
    if p1x > p2x {
        return p1;
    } else if p1x < p2x {
        return p2
    } else {
        return p1;
    }
}

/**
 * Returns a pair of points, where each element is the input point.
 * 
 */
pub fn duplicate_point(p: Point) -> (Point, Point) {
    return (p, p);
}
