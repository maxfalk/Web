module Point
type Point = decimal * decimal


let PointToString (point:Point) =
    (string (fst point), string (snd point))
