module Playfield where

  import Tetromino

  -- A Cell is represented by it's Coord and Mino type
  type Cell = (Int, Int, Shape)

  -- A Playfield is represented by a list of cells that is taken
  type Playfield = [Cell]

  -- Transform to Coords for better usage
  coord :: Playfield -> [Coord]
  coord = map (\(x, y, _) -> (x, y))

  -- Land a Mino
  appendMino :: Playfield -> Mino -> Playfield
  appendMino pf (Mino xs s _) = pf ++ map (\(x, y) -> (x, y, s)) xs

  -- The width of the playfield, left to 0 and right to 9
  fieldWidth :: Int
  fieldWidth = 10

  -- The height of the playfield, zero on the top and 19 at the buttom
  fieldHeight :: Int
  fieldHeight = 20
