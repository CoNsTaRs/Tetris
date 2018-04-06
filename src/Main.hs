module Main where

import           Color
import           Control.Concurrent                (MVar, isEmptyMVar,
                                                    newEmptyMVar, newMVar,
                                                    putMVar, readMVar, swapMVar,
                                                    takeMVar)
import           Control.Concurrent.Suspend.Lifted (msDelay)
import           Control.Concurrent.Timer          (repeatedStart,
                                                    repeatedTimer)
import           Control.Monad                     (join, liftM2, void)
import           Playfield
import           RotationSystem
import           SuperRS
import           System.IO                         (BufferMode (NoBuffering),
                                                    hReady, hSetBuffering,
                                                    hSetEcho, stdin, stdout)
import           System.Sleep                      (sleep)
import           Tetris
import           Tetromino

data Action
  = MVLFT
  | MVRHT
  | MVDWN
  | HRDRP
  | ROTCW
  | ROTCC
  | AHOLD
  | NOACT
  deriving (Eq)

data TetrisMachine = TetrisMachine
  { rotSystem :: RotationSystem
  , playfield :: MVar Playfield
  , currMino  :: MVar Mino
  , locked    :: MVar Bool
  , holded    :: MVar Shape
  , nextList  :: [Shape]
  }

data Env = Env
  { ouptStream :: MVar [String] -- Currently not used
  , inptStream :: MVar [Action]
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
eraseMino (Mino [] _ _) = return ()
eraseMino (Mino ((x, y):as) r s) = do
  eraseCell (x, y)
  eraseMino (Mino as r s)

eraseShadow :: Playfield -> Mino -> IO ()
eraseShadow _ (Mino [] _ _) = return ()
eraseShadow pf mino =
  case snd $ hardDrop pf mino of
    (Mino as _ _) -> eraseShadow' as
      where eraseShadow' :: Area -> IO ()
            eraseShadow' [] = return ()
            eraseShadow' ((x, y):xs) = do
              eraseCell (x, y)
              eraseShadow' xs

printMino :: Mino -> IO ()
printMino (Mino [] _ _) = return ()
printMino (Mino ((x, y):as) s r)
  | x >= 0 && y >= 0 = do
    printCell (x, y, s)
    printMino (Mino as s r)
  | otherwise = printMino (Mino as s r)

printShadow :: Playfield -> Mino -> IO ()
printShadow _ (Mino [] _ _) = return ()
printShadow pf mino =
  case snd $ hardDrop pf mino of
    (Mino as _ _) -> printShadow' as
      where printShadow' :: Area -> IO ()
            printShadow' [] = return ()
            printShadow' ((x, y):xs) = do
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
      (if more
         then getKey'
         else return)
        (char : chars)

getInput :: Env -> IO ()
getInput env = do
  key <- getKey
  isEmpty <- isEmptyMVar (inptStream env)
  if isEmpty
    then void $ putMVar (inptStream env) [action key]
    else do
      inpt <- readMVar (inptStream env)
      void $ swapMVar (inptStream env) (inpt ++ [action key])
  where
    action key =
      case key of
        "\ESC[A" -> ROTCW
        "\ESC[B" -> MVDWN
        "\ESC[C" -> MVRHT
        "\ESC[D" -> MVLFT
        "\SP"    -> HRDRP
        "z"      -> ROTCC
        "x"      -> AHOLD
        _        -> NOACT

getAction :: Env -> IO Action
getAction env = do
  actions <- readMVar (inptStream env)
  if null actions
    then do
      takeMVar (inptStream env)
      return NOACT
    else do
      swapMVar (inptStream env) (tail actions)
      return (head actions)

doAction :: TetrisMachine -> Action -> IO TetrisMachine
doAction tm act = do
  locked' <- readMVar (locked tm)
  if locked'
    then return tm
    else do
      mino <- readMVar (currMino tm)
      pf <- readMVar (playfield tm)
      case act of
        MVLFT ->
          let mino' = moveLft pf mino
           in do updateMino pf mino mino'
                 swapMVar (currMino tm) mino'
                 return tm
        MVRHT ->
          let mino' = moveRht pf mino
           in do updateMino pf mino mino'
                 swapMVar (currMino tm) mino'
                 return tm
        MVDWN ->
          case softDrop pf mino of
            (Left mino') -> do
              updateMino pf mino mino'
              swapMVar (currMino tm) mino'
              return tm
            (Right pf') -> do
              swapMVar (playfield tm) pf'
              swapMVar (locked tm) True
              return tm
        HRDRP ->
          let pf' = (fst $ hardDrop pf mino)
           in do swapMVar (playfield tm) pf'
                 swapMVar (locked tm) True
                 return tm
        ROTCW ->
          let mino' = rotate (rotSystem tm) pf mino CW
           in do updateMino pf mino mino'
                 swapMVar (currMino tm) mino'
                 return tm
        ROTCC ->
          let mino' = rotate (rotSystem tm) pf mino CC
           in do updateMino pf mino mino'
                 swapMVar (currMino tm) mino'
                 return tm
        AHOLD -> do
          isEmpty <- isEmptyMVar (holded tm)
          if isEmpty
            then do
              _ <- swapMVar (locked tm) False
              putMVar (holded tm) (shapeOf mino)
              _ <-
                swapMVar
                  (currMino tm)
                  (rsSpawn (rotSystem tm) (head (nextList tm)))
              mino' <- readMVar (currMino tm)
              updateMino pf mino mino'
              return
                (TetrisMachine
                   (rotSystem tm)
                   (playfield tm)
                   (currMino tm)
                   (locked tm)
                   (holded tm)
                   (tail (nextList tm)))
            else do
              holded' <- readMVar (holded tm)
              _ <- swapMVar (holded tm) (shapeOf mino)
              eraseMino mino
              _ <- swapMVar (currMino tm) (rsSpawn (rotSystem tm) holded')
              mino' <- readMVar (currMino tm)
              updateMino pf mino mino'
              return tm
        _ -> return tm

main :: IO ()
main = do
  hideCursor
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  clearScreen
  clearField
  pf <- newMVar []
  mino <- newMVar (Mino [] ShpI Spw)
  lockDown <- newMVar False
  holdedMino <- newEmptyMVar
  inpt <- newEmptyMVar :: IO (MVar [String])
  oupt <- newEmptyMVar :: IO (MVar [Action])
  inptTimer <- repeatedTimer (return ()) (msDelay 0)
  _ <- repeatedStart inptTimer (getInput (Env inpt oupt)) (msDelay 0)
  gravityStart (Env inpt oupt) lockDown
  generatePhase
    (TetrisMachine superRS pf mino lockDown holdedMino [])
    (Env inpt oupt)
  where
    gravityStart :: Env -> MVar Bool -> IO ()
    gravityStart env' isLocked = do
      timer <- repeatedTimer (return ()) (msDelay 0)
      _ <- repeatedStart timer doSoftDrop (msDelay 1000)
      return ()
      where
        doSoftDrop :: IO ()
        doSoftDrop = do
          p <- readMVar isLocked
          if p
            then return ()
            else do
              isEmpty <- isEmptyMVar (inptStream env')
              if isEmpty
                then void $ putMVar (inptStream env') [MVDWN]
                else do
                  inpt <- readMVar (inptStream env')
                  void $ swapMVar (inptStream env') (inpt ++ [MVDWN])


    generatePhase :: TetrisMachine -> Env -> IO ()
    generatePhase tm env = do
      bag' <- bag
      mino <- newMVar (rsSpawn (rotSystem tm) (head bag'))
      mino' <- readMVar mino
      pf <- readMVar (playfield tm)
      locked' <- newMVar False
      isEmpty <- isEmptyMVar (inptStream env)
      if isEmpty
        then return ()
        else void $ takeMVar (inptStream env)
      let tm' =
            TetrisMachine
              (rotSystem tm)
              (playfield tm)
              mino
              locked'
              (holded tm)
              (tail bag')
       in do printShadow pf mino'
             printMino mino'
             fallingPhase tm' env
      where
        bag :: IO [Shape]
        bag =
          if length (nextList tm) <= 1
            then liftM2 (++) (return $ nextList tm) randomGenerate
            else return (nextList tm)


    fallingPhase :: TetrisMachine -> Env -> IO ()
    fallingPhase tm env = do
      locked' <- readMVar (locked tm)
      if locked'
        then completePhase tm env
        else join $
             liftM2 fallingPhase (getAction env >>= doAction tm) (return env)


    completePhase :: TetrisMachine -> Env -> IO ()
    completePhase tm env = do
      pf <- readMVar (playfield tm)
      sleep 0.03
      printField $ fieldClearedFullLines pf
      sleep 0.1
      printField $ fieldLineCleared pf
      _ <- swapMVar (playfield tm) (fieldLineCleared pf)
      generatePhase tm env
      where
        fieldClearedFullLines :: Playfield -> Playfield
        fieldClearedFullLines pf = clearLines pf $ findFullLines pf
        fieldLineCleared :: Playfield -> Playfield
        fieldLineCleared pf =
          lineClearGravityNaive (fieldClearedFullLines pf) (findFullLines pf)
