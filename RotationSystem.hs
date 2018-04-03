module RotationSystem where

  import Tetromino
  import Playfield

  -- Directions to rotate, clockwise and counter-clockwise
  data Dir = CW | CC

  {-
     A Rotation System defines:

       How tetrominos spawn
       How tetrominos rotate
       What wall kicks tetrominos may perform

     accroding to http://tetris.wikia.com/wiki/Category:Rotation_Systems
  -}

  type Spawn    = Shape -> Mino
  type Rotate   = Dir -> Mino -> Mino
  type WallKick = Playfield -> Mino -> Dir -> Maybe Mino

  data RotationSystem = RS Spawn Rotate WallKick
