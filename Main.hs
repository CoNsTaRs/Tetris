module Main where

  import Control.Monad (liftM2)
  import System.IO (hReady, stdin, stdout, hSetEcho, hSetBuffering, BufferMode(NoBuffering))
  import System.Sleep (sleep)
  import Control.Concurrent (MVar, newMVar, newEmptyMVar, isEmptyMVar, readMVar, putMVar, swapMVar)
  import Control.Concurrent.Timer (repeatedTimer, repeatedStart)
  import Control.Concurrent.Suspend.Lifted (msDelay)
  import Tetromino
  import Playfield
  import RotationSystem
  import SuperRS
  import Tetris
  import Color

  data Action = MVLFT | MVRHT | MVDWN | HRDRP | ROTCW | AHOLD | NOACT
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

  moveCursor :: (Int, Int) -> IO ()
  moveCursor (x, y)
    | x >= 0 && y >= 0 = putStr escapeCode
    | otherwise = moveCursor (21, 0)
    where
      escapeCode :: String
      escapeCode = "\ESC[" ++ show y ++ ";" ++ show ((x * 2) + 1) ++ "H"

  clearScreen :: IO ()
  clearScreen = putStr "\ESC[2J"

  clearField :: IO ()
  clearField = do
    putStr "\ESC[0;0H\ESC[1;37m"
    clearField' emptyField
      where
        emptyField :: [String]
        emptyField = replicate fieldHeight (replicate (fieldWidth * 2) '█')
        clearField' :: [String] -> IO ()
        clearField' = foldr ((*>) . putStrLn) (return ())

  printCell :: Cell -> IO ()
  printCell (x, y, s)
    | x >= 0 && y >= 0 = do
      moveCursor (x, y)
      putStr (colorOf s ++ "██")
    | otherwise = return ()

  printCellShadow :: Coord -> IO ()
  printCellShadow (x, y)
    | x >= 0 && y >= 0 = do
      moveCursor (x, y)
      putStr "\ESC[1;30m██"
    | otherwise = return ()

  eraseCell :: Coord -> IO ()
  eraseCell (x, y)
    | x >= 0 && y >= 0 = do
      moveCursor (x, y)
      putStr "\ESC[1;37m██"
    | otherwise = return ()

  eraseMino :: Mino -> IO ()
  eraseMino (Mino []            _ _) = return ()
  eraseMino (Mino ((x, y) : as) r s) = do
    eraseCell (x, y)
    eraseMino (Mino as r s)

  eraseShadow :: Playfield -> Mino -> IO ()
  eraseShadow _ (Mino [] _ _) = return ()
  eraseShadow pf mino = case snd $ hardDrop pf mino of
    (Mino as _ _) -> eraseShadow' as where
      eraseShadow' :: Area -> IO ()
      eraseShadow' [] = return ()
      eraseShadow' ((x, y) : xs) = do
        eraseCell (x, y)
        eraseShadow' xs

  printMino :: Mino -> IO ()
  printMino (Mino []            _ _) = return ()
  printMino (Mino ((x, y) : as) s r)
    | x >= 0 && y >= 0 = do
      printCell (x, y, s)
      printMino (Mino as s r)
    | otherwise = printMino (Mino as s r)

  printShadow :: Playfield -> Mino -> IO ()
  printShadow _ (Mino [] _ _) = return ()
  printShadow pf mino = case snd $ hardDrop pf mino of
    (Mino as _ _) -> printShadow' as where
      printShadow' :: Area -> IO ()
      printShadow' [] = return ()
      printShadow' ((x, y) : xs) = do
        printCellShadow (x, y)
        printShadow' xs

  updateMino :: Playfield -> Mino -> Mino -> IO ()
  updateMino pf mino mino' = do
    eraseMino mino
    eraseShadow pf mino
    printShadow pf mino'
    printMino mino'

  printField :: Playfield -> IO ()
  printField pf = clearField *> foldr ((*>) . printCell) (return ()) pf

  getKey :: IO String
  getKey = reverse <$> getKey' ""
    where
      getKey' chars = do
        char <- getChar
        more <- hReady stdin
        (if more then getKey' else return) (char : chars)

  getAction :: IO Action
  getAction = do
    ready <- hReady stdin
    if ready then do
      key <- getKey
      case key of
        "\ESC[A" -> return ROTCW
        "\ESC[B" -> return MVDWN
        "\ESC[C" -> return MVRHT
        "\ESC[D" -> return MVLFT
        "\SP"    -> return HRDRP
        "z"      -> return AHOLD
        _        -> return NOACT
    else do
      sndReady <- hReady stdin
      if sndReady then getAction else return NOACT

  doAction :: Env -> Action -> IO Env
  doAction env act = do
    locked' <- readMVar (locked env)
    if locked' then
      return env
    else do
      mino <- readMVar (currMino env)
      pf   <- readMVar (playfield env)
      case act of
        MVLFT -> let mino'= moveLft pf mino in do
          updateMino pf mino mino'
          swapMVar (currMino env) mino'
          return env
        MVRHT -> let mino'= moveRht pf mino in do
          updateMino pf mino mino'
          swapMVar (currMino env) mino'
          return env
        MVDWN -> case softDrop pf mino of
          (Left mino') -> do
            updateMino pf mino mino'
            swapMVar (currMino env) mino'
            return env
          (Right pf')  -> do
            swapMVar (playfield env) pf'
            swapMVar (locked env) True
            return env
        HRDRP -> let pf' = (fst $ hardDrop pf mino) in do
          swapMVar (playfield env) pf'
          swapMVar (locked env) True
          return env
        ROTCW -> let mino'= rotate (rotSystem env) pf mino CW in do
          updateMino pf mino mino'
          swapMVar (currMino env) mino'
          return env
        AHOLD -> do
          isEmpty <- isEmptyMVar (holded env)
          if isEmpty then do
            _ <- swapMVar (locked env) False
            putMVar (holded env) (shapeOf mino)
            _ <- swapMVar (currMino env) (rsSpawn (rotSystem env) (head (nextList env)))
            mino' <- readMVar (currMino env)
            updateMino pf mino mino'
            return (Env (rotSystem env) (playfield env) (currMino env) (locked env) (holded env) (tail (nextList env)))
          else do
            holded' <- readMVar (holded env)
            _ <- swapMVar (holded env) (shapeOf mino)
            eraseMino mino
            _ <- swapMVar (currMino env) (rsSpawn (rotSystem env) holded')
            mino' <- readMVar (currMino env)
            updateMino pf mino mino'
            return env
        _     -> return env

  main :: IO ()
  main = do
    hideCursor
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    clearScreen
    clearField
    w <- newMVar []
    x <- newMVar (Mino [] ShpI Spw)
    y <- newMVar False
    z <- newEmptyMVar
    generatePhase (Env superRS w x y z []) where

      generatePhase :: Env -> IO ()
      generatePhase env = do
        bag' <- bag
        mino <- newMVar (rsSpawn (rotSystem env) (head bag'))
        mino' <- readMVar mino
        pf <- readMVar (playfield env)
        locked' <- newMVar False
        let env' = Env (rotSystem env) (playfield env) mino locked' (holded env) (tail bag') in gravityStart env' *> printShadow pf mino' *> printMino mino' *> fallingPhase env'
        where
          bag :: IO [Shape]
          bag = if length (nextList env) <= 1 then liftM2 (++) (return $ nextList env) randomGenerate else return (nextList env)

          gravityStart :: Env -> IO ()
          gravityStart env' = do
            timer <- repeatedTimer (return ()) (msDelay 0)
            _ <- repeatedStart timer doSoftDrop (msDelay 1000)
            return ()
              where
                doSoftDrop :: IO ()
                doSoftDrop = do
                  pf <- readMVar (playfield env')
                  locked' <- readMVar (locked env')
                  mino <- readMVar (currMino env')
                  case locked' of
                    False -> case softDrop pf mino of
                      (Left  mino') -> do
                        swapMVar (currMino env') mino'
                        eraseMino mino
                        printMino mino'
                      (Right pf')   -> do
                        swapMVar (playfield env') pf'
                        swapMVar (locked env') True
                        printField pf'
                    _ -> return ()

      fallingPhase :: Env -> IO ()
      fallingPhase env = do
        locked' <- readMVar (locked env)
        if locked' then completePhase env else (getAction >>= doAction env) >>= fallingPhase

      completePhase :: Env -> IO ()
      completePhase env = do
        pf <- readMVar (playfield env)
        -- sleep 0.03
        printField $ fieldClearedFullLines pf
        sleep 0.1
        printField $ fieldLineCleared pf
        _ <- swapMVar (playfield env) (fieldLineCleared pf)
        generatePhase env
          where
            fieldClearedFullLines :: Playfield -> Playfield
            fieldClearedFullLines pf = clearLines pf $ findFullLines pf
            fieldLineCleared :: Playfield -> Playfield
            fieldLineCleared pf = lineClearGravityNaive (fieldClearedFullLines pf) (findFullLines pf)
