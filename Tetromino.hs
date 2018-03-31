module Tetromino where

  -- Shapes of Minos, which are I, O, S, Z, L, J and T
  data Shape = ShpI | ShpO | ShpS | ShpZ | ShpL | ShpJ | ShpT

  -- Rotation states of Minos, facing up, right, down and left
  data Rotation = Spw | Rht | Rev | Lft

  -- Coordinate of cells
  type Coord = (Int, Int)

  -- Area that is covered by a Mino, represented by a list of cells
  type Area = [Coord]

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
