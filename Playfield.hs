module Playfield where

  -- List of cells that is taken
  type Playfield = [(Int, Int)]

  -- The width of the playfield, left is 0 and right is 9
  fieldWidth :: Int
  fieldWidth = 10

  -- The height of the playfield, top is zero and buttom is 19
  fieldHeight :: Int
  fieldHeight = 20
