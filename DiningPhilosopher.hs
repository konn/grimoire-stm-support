module Main where
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.IO.Unsafe
import System.Random

queue :: TChan String
queue = unsafePerformIO newTChanIO

data Philosopher = Philosopher { no    :: Int
                               , left  :: Chopsticks
                               , right :: Chopsticks
                               , mode  :: TVar Mode
                               }

type Chopsticks = TVar State
data State = Empty | Available
             deriving (Show, Eq, Ord)
data Mode  = Thinking | Hungry | Eating
             deriving (Show, Eq)

randomDelay :: IO ()
randomDelay = threadDelay =<< randomRIO (1,100000)

msg :: Philosopher -> String -> IO ()
msg phil state =
  atomically $ writeTChan queue $ "Philosopher #" ++ show (no phil) ++ " " ++ state

takeChopsticks :: Chopsticks -> STM ()
takeChopsticks knife = do
  k <- readTVar knife
  check $ k == Available
  writeTVar knife Empty

philosopher :: Philosopher -> IO ThreadId
philosopher phil = forkIO $ forever $ do
  st <- readTVarIO $ mode phil
  case st of
    Thinking -> do
      msg phil "is thinking."
      atomically $ writeTVar (mode phil) Hungry
    Hungry -> do
      msg phil "is getting hungry..."
      atomically $ do
        takeChopsticks (left phil)
        takeChopsticks (right phil)
        writeTVar (mode  phil) Eating
      msg phil "took both knife."
      msg phil "eating..."
    Eating -> do
      atomically $ do
        writeTVar (left  phil) Available
        writeTVar (right phil) Available
        writeTVar (mode  phil) Thinking
      msg phil "returned the both knives."
  randomDelay

mkPhilosophers :: Int -> IO [Philosopher]
mkPhilosophers n
    | n <= 0    = return []
    | n == 1    = liftM pure $
                  Philosopher 1 <$> newTVarIO Available
                                <*> newTVarIO Available
                                <*> newTVarIO Hungry
    | otherwise = do
  ks@(k:rs) <- replicateM n $ newTVarIO Available
  zipWithM mkPhil [1..] (zip ks (rs ++ [k]))
  where
    mkPhil num (l, r) = do
      state <- newTVarIO Hungry
      return (Philosopher num l r state)

main :: IO ()
main = do
  ps <- mkPhilosophers 5
  tids <- mapM philosopher ps
  hoge <- forkIO $ forever $
    atomically (readTChan queue) >>= putStrLn
  threadDelay (5 * 10^6)
  mapM_ killThread tids
  atomically $ isEmptyTChan queue >>= check
  killThread hoge
