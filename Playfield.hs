module Playfield where

  -- A Playfield is represented by a list of cells that is taken
  type Playfield = [(Int, Int)]

  -- The width of the playfield, left to 0 and right to 9
  fieldWidth :: Int
  fieldWidth = 10

  -- The height of the playfield, zero on the top and 19 at the buttom
  fieldHeight :: Int
  fieldHeight = 20
