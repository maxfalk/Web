module Point
type Point = float * float


let PointToString (point:Point) =
    (string (fst point), string (snd point))
