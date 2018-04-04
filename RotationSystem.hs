module RotationSystem where

  import Tetromino
  import Playfield

  -- Directions to rotate, clockwise and counter-clockwise
  data Dir = CW | CC

  {-
     A Rotation System should define:

       * How tetrominos spawn
       * How tetrominos rotate
       * What wall kicks tetrominos may perform

     Accroding to https://tetris.wiki/Rotation_system
  -}

  type Spawn    = Shape -> Mino
  type Rotate   = Dir -> Mino -> Mino
  type WallKick = Playfield -> Mino -> Dir -> Maybe Mino

  data RotationSystem = RS {
    rsSpawn    :: Spawn,
    rsRotate   :: Rotate,
    rsWallKick :: WallKick
  }
