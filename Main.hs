module Main where

  import System.IO
  import System.Sleep (sleep)
  import Control.Concurrent (forkIO, MVar, newMVar, newEmptyMVar, readMVar, swapMVar)
  import Control.Concurrent.Timer (repeatedTimer, repeatedStart)
  import Control.Concurrent.Suspend.Lifted (msDelay)
  import Tetromino
  import Playfield
  import RotationSystem
  import SuperRS
  import Tetris

  data Action = MVLFT | MVRHT | MVDWN | HRDRP | ROTCW | NOACT
    deriving (Eq)

  data Env = Env {
    rotSystem :: RotationSystem,
    playfield :: MVar Playfield,
    currMino  :: MVar Mino,
    locked    :: MVar Bool,
    holded    :: MVar Shape,
    nextList  :: [Shape]
  }

  hideCursor :: IO ()
  hideCursor = putStr "\ESC[?25l"

  moveCursor :: Int -> Int -> IO ()
  moveCursor x y  = putStr escapeCode
    where
      escapeCode :: String
      escapeCode = "\ESC[" ++ show (y + 1) ++ ";" ++ show ((x + 1) * 2) ++ "H"

  clearScreen :: IO ()
  clearScreen = putStr "\ESC[2J"

  printCell :: (Int, Int) -> IO ()
  printCell (x, y)
    | x >= 0 && y >= 0 = moveCursor x y *> putStr "██"
    | otherwise = return ()

  eraseMino :: Mino -> IO ()
  eraseMino (Mino []            _ _) = return ()
  eraseMino (Mino ((x, y) : as) r s) = moveCursor x y *> putStr "  " *> eraseMino (Mino as r s)

  printMino :: Mino -> IO ()
  printMino (Mino []            _ _) = return ()
  printMino (Mino (x : xs) r s) = printCell x *> printMino (Mino xs r s)

  printField :: Playfield -> IO ()
  printField pf = clearScreen *> foldr ((*>) . printCell) (return ()) pf

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

  doAction :: Env -> Action -> IO Env
  doAction env act = do
    mino <- readMVar (currMino env)
    pf   <- readMVar (playfield env)
    case act of
      MVLFT -> let mino'= moveLft pf mino in eraseMino mino *> printMino mino' *> swapMVar (currMino env) mino' *> return env
      MVRHT -> let mino'= moveRht pf mino in eraseMino mino *> printMino mino' *> swapMVar (currMino env) mino' *> return env
      MVDWN -> case softDrop pf mino of
        (Left mino') -> eraseMino mino *> printMino mino' *> swapMVar (currMino env) mino' *> return env
        (Right pf')  -> swapMVar (playfield env) pf' *> swapMVar (locked env) True *> return env
      HRDRP -> let pf' = (hardDrop pf mino) in swapMVar (playfield env) pf' *> swapMVar (locked env) True *> return env
      ROTCW -> let  mino'= rotate (rotSystem env) pf mino CW in eraseMino mino *> printMino mino' *> swapMVar (currMino env) mino' *> return env
      _     -> return env

  main :: IO ()
  main = do
    hideCursor
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    clearScreen
    w <- newMVar []
    x <- newMVar (Mino [] ShpI Spw)
    y <- newMVar False
    z <- newEmptyMVar
    generatePhase (Env superRS w x y z []) where

      generatePhase :: Env -> IO ()
      generatePhase env = do
        bag' <- bag
        mino <- newMVar (rsSpawn (rotSystem env) (head bag'))
        locked' <- newMVar False
        _ <- forkIO gravityStart
        fallingPhase (Env (rotSystem env) (playfield env) mino locked' (holded env) (tail bag'))
        where
          bag :: IO [Shape]
          bag = if null (nextList env) then randomGenerate else return (nextList env)

          gravityStart :: IO ()
          gravityStart = do
            timer <- repeatedTimer doSoftDrop (msDelay 500)
            _ <- repeatedStart timer doSoftDrop (msDelay 500)
            return ()
              where
                doSoftDrop :: IO ()
                doSoftDrop = do
                  pf <- readMVar (playfield env)
                  locked' <- readMVar (locked env)
                  mino <- readMVar (currMino env)
                  case locked' of
                    False -> case softDrop pf mino of
                      (Left  mino') -> swapMVar (currMino env) mino' *> eraseMino mino *> printMino mino'
                      (Right pf')   -> swapMVar (playfield env) pf' *> swapMVar (locked env) True *> printField pf'
                    _ -> return ()

      fallingPhase :: Env -> IO ()
      fallingPhase env = do
        locked' <- readMVar (locked env)
        case locked' of
          True -> completePhase env
          _    -> f >>= fallingPhase
          where
            f :: IO Env
            f = getAction >>= doAction env

      completePhase :: Env -> IO ()
      completePhase env = do
        pf <- readMVar (playfield env)
        -- sleep 0.03
        printField $ fieldClearedFullLines pf
        sleep 0.1
        printField $ fieldLineCleared pf
        swapMVar (playfield env) (fieldLineCleared pf)
        generatePhase env
          where
            fieldClearedFullLines :: Playfield -> Playfield
            fieldClearedFullLines pf = clearLines pf $ findFullLines pf
            fieldLineCleared :: Playfield -> Playfield
            fieldLineCleared pf = lineClearGravityNaive (fieldClearedFullLines pf) (findFullLines pf)
