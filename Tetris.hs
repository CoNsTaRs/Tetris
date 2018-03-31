module Tetris where

  import Data.List

  import Tetromino
  import Playfield
  import RotationSystem
  
  moveLft :: Playfield -> Area -> Area
  moveLft pf ar = if tryLft ar then map shiftLft ar else ar where
    
    tryLft :: Area -> Bool
    tryLft area = case area of
      ((0, _) : _) -> False
      ((x, y) : []) -> not (elem (x - 1, y) pf)
      ((x, y) : as) -> (not (elem (x - 1, y) pf)) && (tryLft as)
    
    shiftLft :: Coord -> Coord
    shiftLft (x, y) = (x - 1, y)
  
  moveRht :: Playfield -> Area -> Area
  moveRht pf ar = if tryRht ar then map shiftRht ar else ar where
    
    tryRht :: Area -> Bool
    tryRht area = case area of
      ((fieldWidth, _) : _) -> False
      ((x, y)          : []) -> not (elem (x + 1, y) pf)
      ((x, y)          : as) -> (not (elem (x + 1, y) pf)) && (tryRht as)
    
    shiftRht :: Coord -> Coord
    shiftRht (x, y) = (x + 1, y)
  
  
  softDorp :: Playfield -> Area -> Area
  softDorp pf ar = if tryDwn ar then map shiftDwn ar else ar where

    tryDwn :: Area -> Bool
    tryDwn ar = case ar of
      ((_, fieldHeight) : _)  -> False
      ((x, y)           : []) -> not (elem (x, y + 1) pf)
      ((x, y)           : as) -> (not (elem (x, y + 1) pf)) && (tryDwn as)
    
    shiftDwn :: Coord -> Coord
    shiftDwn (x, y) = (x, y + 1)
  
  
  hardDrop :: Playfield -> Area -> Playfield
  hardDrop pf ar = pf ++ (fmap (drop height) ar) where

    drop :: Int -> Coord -> Coord
    drop n (x, y) = (x, y + n)

    height :: Int
    height = foldl max 0 (map cellHeight ar) where
      
      cellHeight :: Coord -> Int
      cellHeight (_, y) = foldl max 0 (map snd (filter ((y ==) . fst) pf))
  
  
  rotateCC :: Playfield -> Shape -> Rotation -> Area -> Area
  rotateCC _ ShpO _ x = x
  rotateCC pf s r a = applyWallKick attempt where

    rotated :: Area
    rotated = tetromRotCC s r a

    attempt :: Maybe Area
    attempt = wallKick pf s r CC rotated

    applyWallKick :: Maybe Area -> Area
    applyWallKick Nothing  = a
    applyWallKick (Just x) = x
  
  
  isFillLine :: Int -> Int -> Playfield -> Bool
  isFillLine 0 _ _             = True
  isFillLine _ _ (_ : [])      = False
  isFillLine n l ((_, y) : xs) = isFillLine (if y == l then (n + 1) else n) l xs

  
  clearLines :: Playfield -> [Int] -> Playfield
  clearLines xs (y : ys) = clearLines (clearLine xs y) ys where

    clearLine :: Playfield -> Int -> Playfield
    clearLine xs@((_, y) : []) n = if n == y then [] else xs
    clearLine    ((x, y) : ys) n = if n == y then clearLine ys n else (x, y) : (clearLine ys n)
  
  
  dropLine :: Playfield -> Int -> Playfield
  dropLine pf l = map dropHigherLines pf where

    dropHigherLines :: Coord -> Coord
    dropHigherLines (x, y) = (x, if y < l then y - 1 else y)
  
  
  dropLines :: Playfield -> [Int] -> Playfield
  dropLines pf xs = dropLines' pf (sort xs) where

    dropLines' :: Playfield -> [Int] -> Playfield
    dropLines' _ [] = []
    dropLines' pf (x : xs) = dropLines' (dropLine pf x) xs
