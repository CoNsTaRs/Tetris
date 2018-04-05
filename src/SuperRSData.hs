module SuperRSData where

  import Tetromino
  import RotationSystem

  -- Difference between two coords
  type Offset = (Int, Int)

  -- The original covered area of each Mino for SuperRS
  -- accroding to https://tetris.wiki/SRS#Spawn_Orientation_and_Location
  origStateSRS :: Shape -> Rotation -> Area
  origStateSRS shape rotation = case (shape, rotation) of
    (ShpI, Spw) -> [(3,  0), (4,  0), (5,  0), (6, 0)]
    (ShpI, Rht) -> [(5, -1), (5,  0), (5,  1), (5, 2)]
    (ShpI, Rev) -> [(3,  1), (4,  1), (5,  1), (6, 1)]
    (ShpI, Lft) -> [(4, -1), (4,  0), (4,  1), (4, 2)]
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
    (ShpL, Rev) -> [(3,  0), (4,  0), (5,  0), (3, 1)]
    (ShpL, Lft) -> [(3, -1), (4, -1), (4,  0), (4, 1)]
    (ShpJ, Spw) -> [(3, -1), (3,  0), (4,  0), (5, 0)]
    (ShpJ, Rht) -> [(4, -1), (5, -1), (4,  0), (4, 1)]
    (ShpJ, Rev) -> [(3,  0), (4,  0), (5,  0), (5, 1)]
    (ShpJ, Lft) -> [(4, -1), (4,  0), (3,  1), (4, 1)]
    (ShpT, Spw) -> [(4, -1), (3,  0), (4,  0), (5, 0)]
    (ShpT, Rht) -> [(4, -1), (4,  0), (5,  0), (4, 1)]
    (ShpT, Rev) -> [(3,  0), (4,  0), (5,  0), (4, 1)]
    (ShpT, Lft) -> [(4, -1), (3,  0), (4,  0), (4, 1)]

  -- Wall kick data for SuperRS
  -- according to https://tetris.wiki/SRS#Wall_Kicks
  wallKickDataSRS :: Shape -> Rotation -> Dir -> [Offset]
  wallKickDataSRS s r d = case (s, (r, d)) of
    (ShpO, (_,   _))  -> [(0, 0)]
    (ShpI, (Rht, CW)) -> [(0, 0), (-2, 0), ( 1, 0), (-2,  1), ( 1, -2)]
    (ShpI, (Spw, CC)) -> [(0, 0), ( 2, 0), (-1, 0), ( 2, -1), (-1,  2)]
    (ShpI, (Rev, CW)) -> [(0, 0), (-1, 0), ( 2, 0), (-1, -2), ( 2,  1)]
    (ShpI, (Rht, CC)) -> [(0, 0), ( 1, 0), (-2, 0), ( 1,  2), (-2, -1)]
    (ShpI, (Lft, CW)) -> [(0, 0), ( 2, 0), (-1, 0), ( 2, -1), (-1,  2)]
    (ShpI, (Rev, CC)) -> [(0, 0), (-2, 0), ( 1, 0), (-2,  1), ( 1, -2)]
    (ShpI, (Spw, CW)) -> [(0, 0), ( 1, 0), (-2, 0), ( 1,  2), (-2, -1)]
    (ShpI, (Lft, CC)) -> [(0, 0), (-1, 0), ( 2, 0), (-1, -2), ( 2,  1)]
    (_, (Rht, CW)) -> [(0, 0), (-1, 0), (-1, -1), (0,  2), (-1,  2)]
    (_, (Spw, CC)) -> [(0, 0), ( 1, 0), ( 1,  1), (0, -2), ( 1, -2)]
    (_, (Rev, CW)) -> [(0, 0), ( 1, 0), ( 1,  1), (0, -2), ( 1, -2)]
    (_, (Rht, CC)) -> [(0, 0), (-1, 0), (-1, -1), (0,  2), (-1,  2)]
    (_, (Lft, CW)) -> [(0, 0), ( 1, 0), ( 1, -1), (0,  2), ( 1,  2)]
    (_, (Rev, CC)) -> [(0, 0), (-1, 0), (-1,  1), (0, -2), (-1, -2)]
    (_, (Spw, CW)) -> [(0, 0), (-1, 0), (-1,  1), (0, -2), (-1, -2)]
    (_, (Lft, CC)) -> [(0, 0), ( 1, 0), ( 1, -1), (0,  2), ( 1,  2)]
