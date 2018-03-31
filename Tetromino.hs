module Tetromino where

  -- Shapes of Minos, which are I, O, S, Z, L, J and T
  data Shape = ShpI | ShpO | ShpS | ShpZ | ShpL | ShpJ | ShpT

  -- Rotations of Minos, facing up, right, down and left
  data Rotation = Spw | Rht | Rev | Lft

  -- Directions to rotate, clockwise and counter-clockwise
  data Dir = CW | CC

  -- Coordinate of cells
  type Coord = (Int, Int)

  -- Area that is covered by a Mino, represented by a list of cells
  type Area = [Coord]

  -- The original covered area of each Mino
  tetromState :: Shape -> Rotation -> Area
  tetromState shape rotation = case (shape, rotation) of
    (ShpI, Spw) -> [(2,  0), (3,  0), (4,  0), (5, 0)]
    (ShpI, Rht) -> [(5, -3), (5, -2), (5, -1), (5, 0)]
    (ShpI, Rev) -> [(2,  1), (2,  1), (2,  1), (2, 1)]
    (ShpI, Lft) -> [(4, -3), (4, -2), (4, -1), (4, 0)]
    (ShpO, _)   -> [(4, -1), (5, -1), (4,  0), (5, 0)]
    (ShpS, Spw) -> [(4, -1), (5, -1), (3,  0), (4, 0)]
    (ShpS, Rht) -> [(4, -1), (4,  0), (5,  0), (5, 1)]
    (ShpS, Rev) -> [(4,  0), (5,  0), (3,  1), (4, 1)]
    (ShpS, Lft) -> [(3, -1), (3,  0), (4,  0), (4, 1)]
    (ShpZ, Spw) -> [(3, -1), (4, -1), (4,  0), (5, 0)]
    (ShpZ, Rht) -> [(5, -1), (4,  0), (5,  0), (4, 1)]
    (ShpZ, Rev) -> [(3,  0), (4,  0), (4,  1), (5, 1)]
    (ShpZ, Lft) -> [(4, -1), (3,  0), (4,  0), (3, 1)]
    (ShpL, Spw) -> [(5, -1), (3,  0), (4,  0), (5, 0)]
    (ShpL, Rht) -> [(4, -1), (4,  0), (4,  1), (5, 1)]
    (ShpL, Rev) -> [(3,  0), (4,  0), (5,  0), (5, 1)]
    (ShpL, Lft) -> [(3, -1), (4, -1), (4,  0), (5, 0)]
    (ShpJ, Spw) -> [(3, -1), (3,  0), (4,  0), (5, 0)]
    (ShpJ, Rht) -> [(4, -1), (5, -1), (4,  0), (4, 1)]
    (ShpJ, Rev) -> [(3,  0), (4,  0), (5,  0), (5, 1)]
    (ShpJ, Lft) -> [(4, -1), (4,  0), (3,  1), (4, 1)]
    (ShpT, Spw) -> [(4, -1), (3,  0), (4,  0), (5, 0)]
    (ShpT, Rht) -> [(4, -1), (4,  0), (5,  0), (4, 1)]
    (ShpT, Rev) -> [(3,  0), (4,  0), (5,  0), (4, 1)]
    (ShpT, Lft) -> [(4, -1), (3,  0), (4,  0), (4, 1)]

  -- The initial covered area of each Mino
  tetromInit :: Shape -> Area
  tetromInit shape = tetromState shape Spw

  -- Iterate rotations clockwise
  rotationNext :: Rotation -> Rotation
  rotationNext r = case r of
    Spw -> Rht
    Rht -> Rev
    Rev -> Lft
    Lft -> Rht

  -- Iterate rotations counter-clockwise
  rotationPrev :: Rotation -> Rotation
  rotationPrev r = case r of
    Spw -> Lft
    Rht -> Spw
    Rev -> Rht
    Lft -> Rev

  -- Calculate the new covered area after a rotate
  tetromRotate :: Shape -> Rotation -> Dir -> Area -> Area
  tetromRotate s r d ((x, y) : _) = fmap (applyOffset offset) rotatedOrig where

    rotatedOrig :: Area
    rotatedOrig = case d of
      CC -> tetromState s (rotationNext r)
      CW -> tetromState s (rotationPrev r)

    calcOffset :: Coord -> Coord -> Coord
    calcOffset (x, y) (p, q) = (x - y, p - q)

    applyOffset :: Coord -> Coord -> Coord
    applyOffset (x, y) (p, q) = (x + p, y + q)

    offset :: Coord
    offset = calcOffset (x, y) (head (tetromState s r))
  