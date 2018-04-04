module Main where

  import System.IO
  import Tetromino
  import Playfield
  import RotationSystem
  import SuperRS
  import Tetris

  data Action = MVLFT | MVRHT | MVDWN | HRDRP | ROTCW | NOACT
    deriving (Eq)

  hideCursor :: IO ()
  hideCursor = putStr "\ESC[?25l"

  moveCursor :: Int -> Int -> IO ()
  moveCursor x y  = putStr escapeCode
    where
      escapeCode :: String
      escapeCode = "\ESC[" ++ show (y + 1) ++ ";" ++ show ((x + 1) * 2) ++ "H"

  eraseMino :: Mino -> IO ()
  eraseMino (Mino []            _ _) = return ()
  eraseMino (Mino ((x, y) : as) r s) = moveCursor x y *> putStr "  " *> eraseMino (Mino as r s)

  printMino :: Mino -> IO ()
  printMino (Mino []            _ _) = return ()
  printMino (Mino ((x, y) : as) r s)
    | x >= 0 && y >= 0 = moveCursor x y *> putStr "██" *> printMino (Mino as r s)
    | otherwise = return ()

  getKey :: IO String
  getKey = reverse <$> getKey' ""
    where
      getKey' chars = do
        char <- getChar
        more <- hReady stdin
        (if more then getKey' else return) (char : chars)

  getAction :: IO Action
  getAction = do
    key <- getKey
    case key of
      "\ESC[A" -> return ROTCW
      "\ESC[B" -> return MVDWN
      "\ESC[C" -> return MVRHT
      "\ESC[D" -> return MVLFT
      "\SP"    -> return HRDRP
      _        -> return NOACT

  doAction :: RotationSystem -> Playfield -> Mino -> Action -> IO (Playfield, Mino)
  doAction rs pf mino act
    | act == MVLFT = let mino'= moveLft pf mino in eraseMino mino *> printMino mino' *> return (pf, mino')
    | act == MVRHT = let mino'= moveRht pf mino in eraseMino mino *> printMino mino' *> return (pf, mino')
    | act == MVDWN = let mino'= softDrop pf mino in eraseMino mino *> printMino mino' *> return (pf, mino')
    -- | act == HRDRP = eraseMino mino *> printMino mino' where mino'= (hardDrop pf mino)
    | act == ROTCW = let  mino'= rotate rs pf mino CW in eraseMino mino *> printMino mino' *> return (pf, mino')
    | otherwise    = return (pf, mino)

  main :: IO ()
  main = do
    hideCursor
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    generatePhase superRS [] where
      locked :: Bool
      locked = False

      generatePhase :: RotationSystem -> Playfield -> IO ()
      generatePhase rs@(RS spawn _ _) pf = mino >>= f rs pf
        where
          bag :: IO [Shape]
          bag = randomGenerate
          shape :: IO Shape
          shape = fmap head bag
          mino :: IO Mino
          mino = fmap spawn shape
          f :: RotationSystem -> Playfield -> Mino -> IO ()
          f x y z = fallingPhase x (y, z)

      fallingPhase :: RotationSystem -> (Playfield, Mino) -> IO ()
      fallingPhase rs (pf, mino)
        | locked = completePhase rs pf
        | otherwise = f >>= fallingPhase rs
          where
            f :: IO (Playfield, Mino)
            f = getAction >>= doAction rs pf mino

      completePhase :: RotationSystem -> Playfield -> IO ()
      completePhase = generatePhase  --  printField (lineClearGravity $ clearLine pf $ findFullLine pf) >>=
      
