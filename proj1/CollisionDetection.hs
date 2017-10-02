{- 
  File      :   CollisionDetection.hs 
  Copyright :   (c) Gianni Chen, 10/01/17 
  Contains types for representing 2D objects and collision functions for them.
  
  Sources   :   http://devmag.org.za/2009/04/13/basic-collision-detection-in-2d-part-1/
                http://devmag.org.za/2009/04/17/basic-collision-detection-in-2d-part-2/
  
  Notes     :   No idea if it compiles; could not test it since my gcc is broken
                Could not test since gcc is broken
-}

{- Typedef for representing a Point in (x, y) -}
type Point = (Double, Double)

{- Typedef for representing a LineSegment -}
type LineSegment = (Point, Point)

{- Typedef for representing a BoundingBox, assuming only rectangles -}
type BoundingBox = (LineSegment, LineSegment, LineSegment, LineSegment)

{- Typedef for representing a Circle -}
type Circle = (Point, Double)

{- Checks if two Bounding Boxes collide -}
rectsIntersect :: BoundingBox -> BoundingBox -> Bool
rectsIntersect (top1, right1, bot1, left1) (top2, right2, bot2, left2) =

    let bottomCollision = snd (fst bot1) < snd (fst top2)
        topCollision = snd (fst top1) > snd (fst bot2)
        leftCollision = fst (fst left1) > fst (fst right2)
        rightCollision = fst (fst right1) < fst (fst left2)

    in not (bottomCollision || topCollision || leftCollision || rightCollision)

{- Checks if two Line Segments collide -}
linesIntersect :: LineSegment -> LineSegment -> Bool
linesIntersect (a, b) (c, d) =
    let denom = (-) (((-) (snd d) (snd c)) * ((-) (fst b) (fst a))) (((-) (fst d) (fst c)) * ((-) (snd b) (snd a)))
    in (denom /= 0)

{- Checks if a Line collides with a Bounding Box -}
rectLineIntersect :: LineSegment -> BoundingBox -> Bool
rectLineIntersect line (top, right, bot, left) =

    let leftBound = fst (fst left)
        rightBound = fst (fst right)
        topBound = snd (fst top)
        botBound = snd (fst bot)
        x1 = fst (fst line)
        y1 = snd (fst line)
        x2 = fst (snd line)
        y2 = snd (snd line)

        collision = if (x1 > leftBound && x1 < rightBound && x1 > botBound && x1 < topBound) then True else 
            if (x2 > leftBound && x2 < rightBound && y2 > botBound && y2 < topBound) then True else
                (linesIntersect line top || linesIntersect line right || linesIntersect line bot || linesIntersect line left)
        in collision

{- Checks if two Circles collide -}
circlesIntersect :: Circle -> Circle -> Bool
circlesIntersect circ1 circ2 =

    let distX = fst (fst circ1) - fst (fst circ2)
        distY = snd (fst circ1) - snd (fst circ2)
        dist = sqrt((distX * distX) + (distY * distY))

    in dist <= (snd circ1 + snd circ2)

{- Returns the first Point from the second Point's point of reference -}
subtractPoints :: Point -> Point -> Point
subtractPoints (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

{- Checks if a Line Segment intersects a Circle -}
circleLineIntersect :: LineSegment -> Circle -> Bool
circleLineIntersect (p1, p2) (center, r) =

    let localP1 = subtractPoints p1 center
        localP2 = subtractPoints p2 center
        localDiff = subtractPoints localP2 localP1
        a = (fst localDiff) * (fst localDiff) + (snd localDiff) * (snd localDiff)
        b = 2 * ((fst localDiff * fst localP1) + (snd localDiff * snd localP1))
        c = (-) ((fst localP1 * fst localP1) + (snd localP1 * snd localP1)) (r * r)
        delta = (-) (b * b) (4 * a * c)

    in not (delta < 0)
