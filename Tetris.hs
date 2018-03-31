module Tetris where

  import Data.List
  import Tetromino
  import Playfield
  import RotationSystem
  import SuperRS


  -- Left shift for one step
  moveLft :: Playfield -> Area -> Area
  moveLft pf ar = if tryLft ar then map shiftLft ar else ar where

    tryLft :: Area -> Bool
    tryLft area = case area of
      ((0, _) : _)  -> False
      ((x, y) : []) -> not (elem (x - 1, y) pf)
      ((x, y) : as) -> (not (elem (x - 1, y) pf)) && (tryLft as)

    shiftLft :: Coord -> Coord
    shiftLft (x, y) = (x - 1, y)


  -- Right shift for one step
  moveRht :: Playfield -> Area -> Area
  moveRht pf ar = if tryRht ar then map shiftRht ar else ar where

    tryRht :: Area -> Bool
    tryRht area = case area of
      ((fieldWidth, _) : _)  -> False
      ((x, y)          : []) -> not (elem (x + 1, y) pf)
      ((x, y)          : as) -> (not (elem (x + 1, y) pf)) && (tryRht as)

    shiftRht :: Coord -> Coord
    shiftRht (x, y) = (x + 1, y)


  -- Soft drop
  softDorp :: Playfield -> Area -> Area
  softDorp pf ar = if tryDwn ar then map shiftDwn ar else ar where

    tryDwn :: Area -> Bool
    tryDwn ar = case ar of
      ((_, fieldHeight) : _)  -> False
      ((x, y)           : []) -> not (elem (x, y + 1) pf)
      ((x, y)           : as) -> (not (elem (x, y + 1) pf)) && (tryDwn as)

    shiftDwn :: Coord -> Coord
    shiftDwn (x, y) = (x, y + 1)


  -- Hard drop
  hardDrop :: Playfield -> Area -> Playfield
  hardDrop pf ar = pf ++ (fmap (drop height) ar) where

    drop :: Int -> Coord -> Coord
    drop n (x, y) = (x, y + n)

    height :: Int
    height = foldl max 0 (map cellHeight ar) where

      cellHeight :: Coord -> Int
      cellHeight (_, y) = foldl max 0 (map snd (filter ((y ==) . fst) pf))


  -- Rotate clockwise or counter-clockwise
  rotate :: RotationSystem -> Playfield -> Shape -> Rotation -> Dir -> Area -> Area
  rotate _ _ ShpO _ _ x = x
  rotate (RS _ rsRotate rsWallKick) pf s r d a = applyWallKick attempt where

    rotated :: Area
    rotated = rsRotate s r d a

    attempt :: Maybe Area
    attempt = rsWallKick pf s r d rotated

    applyWallKick :: Maybe Area -> Area
    applyWallKick Nothing  = a
    applyWallKick (Just x) = x


  -- Find lines to clear after a Mino lands
  findFullLines :: Playfield -> [Int]
  findFullLines pf = findFullLines' pf [] where

    findFullLines' :: Playfield -> [Int] -> [Int]
    findFullLines' ((x, y) : ps) ls = case (ps, ls) of
      (_, []) -> if isFullLine (fieldWidth - 1) y ps then y : [] else []
      ([], _) -> if elem y ls then ls else if isFullLine (fieldWidth - 1) y ps then y : ls else ls
      (_, _)  -> if elem y ls then findFullLines' ps ls else findFullLines' ps (if isFullLine (fieldWidth - 1) y ps then y : ls else ls)

    isFullLine :: Int -> Int -> Playfield -> Bool
    isFullLine 0 _ _             = True
    isFullLine _ _ (_ : [])      = False
    isFullLine n l ((_, y) : xs) = isFullLine (if y == l then (n + 1) else n) l xs


  -- To clear lines from the line numbers given
  clearLines :: Playfield -> [Int] -> Playfield
  clearLines xs (y : ys) = clearLines (clearLine xs y) ys where

    clearLine :: Playfield -> Int -> Playfield
    clearLine xs@((_, y) : []) n = if n == y then [] else xs
    clearLine    ((x, y) : ys) n = if n == y then clearLine ys n else (x, y) : (clearLine ys n)


  -- Naive line clear gravity
  lineClearGravityNaive :: Playfield -> [Int] -> Playfield
  lineClearGravityNaive pf xs = removeLines pf (sort xs) where

    removeLine :: Playfield -> Int -> Playfield
    removeLine pf l = map dropHigherLines pf where

      dropHigherLines :: Coord -> Coord
      dropHigherLines (x, y) = (x, if y < l then y - 1 else y)

    removeLines :: Playfield -> [Int] -> Playfield
    removeLines pf []       = pf
    removeLines pf (x : xs) = removeLines (removeLine pf x) xs
