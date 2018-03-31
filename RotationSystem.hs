module RotationSystem (SpawnState, Rotate, WallKick, RotationSystem(RS), Dir(CW,CC)) where

  import Tetromino
  import Playfield

  {-
     A Rotation System defines:

       How tetrominos spawn
       How tetrominos rotate
       What wall kicks tetrominos may perform

     accroding to http://tetris.wikia.com/wiki/Category:Rotation_Systems
  -}

  type SpawnState = Shape -> Area
  type Rotate     = Shape -> Rotation -> Dir -> Area -> Area
  type WallKick   = Playfield -> Shape -> Rotation -> Dir -> Area -> Maybe Area

  data RotationSystem = RS SpawnState Rotate WallKick

  -- Directions to rotate, clockwise and counter-clockwise
  data Dir = CW | CC
