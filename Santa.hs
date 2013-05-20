{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import Control.Concurrent
import Control.Concurrent.STM
import Data.Typeable
import Control.Monad
import System.Random
import Control.Applicative

elf1, reindeer1 :: TChan String -> Group -> Int -> IO ()
elf1 q group eid = helper1 group (meetInStudy q eid)
reindeer1 q group rid = helper1 group (deliverToys q rid)

msg :: TChan String -> String -> IO ()
msg q str = atomically $ writeTChan q str

meetInStudy :: TChan String -> Int -> IO ()
meetInStudy q eid = msg q $ "Elf " ++ show eid ++ " meeting in study"

deliverToys :: TChan String -> Int -> IO ()
deliverToys q rid = msg q $ "Reindeer " ++ show rid ++ " delivering toys"

helper1 :: Group -> IO () -> IO ()
helper1 group task = do
  (in_gate, out_gate) <- joinGroup group
  passGate in_gate
  task
  passGate out_gate

data Gate = MkGate Int (TVar Int)
          deriving (Typeable)

newGate :: Int -> STM Gate
newGate n = do
  tv <- newTVar 0
  return $ MkGate n tv

passGate :: Gate -> IO ()
passGate (MkGate n tv) = atomically $ do
  nLeft <- readTVar tv
  check (nLeft > 0)
  writeTVar tv (nLeft - 1)

operateGate :: Gate -> IO ()
operateGate (MkGate n tv) = do
  atomically $ writeTVar tv n
  atomically $ do
    nLeft <- readTVar tv
    check (nLeft == 0)

data Group = MkGroup Int (TVar (Int, Gate, Gate))
           deriving (Eq, Typeable)

joinGroup :: Group -> IO (Gate, Gate)
joinGroup (MkGroup n tv) = atomically $ do
  (nLeft, g1, g2) <- readTVar tv
  check (nLeft > 0)
  writeTVar tv (nLeft - 1, g1, g2)
  return (g1, g2)

newGroup :: Int -> IO Group
newGroup n = atomically $ do
  g1 <- newGate n
  g2 <- newGate n
  tv <- newTVar (n, g1, g2)
  return (MkGroup n tv)

awaitGroup :: Group -> STM (Gate, Gate)
awaitGroup (MkGroup n tv) = do
  (nLeft, g1, g2) <- readTVar tv
  check (nLeft == 0)
  g1' <- newGate n
  g2' <- newGate n
  writeTVar tv (n, g1', g2')
  return (g1, g2)

elf :: TChan String -> Group -> Int -> IO ThreadId
elf q gp eid = forkIO $ forever $ do
  elf1 q gp eid
  randomDelay

randomDelay :: IO ()
randomDelay = randomRIO (1, 100000) >>= threadDelay

main :: IO ()
main = do
  q <- newTChanIO
  lid <- forkIO $ forever $ atomically (readTChan q) >>= putStrLn
  elfGroup <- newGroup 3
  etids <- mapM (elf q elfGroup) [1..10]
  reinGroup <- newGroup 9
  rtids <- mapM (reindeer q reinGroup) [1..9]
  forM_ [1..] $ santa q elfGroup reinGroup
  mapM_ killThread (etids ++ rtids)
  atomically $ isEmptyTChan q >>= check
  killThread lid

reindeer :: TChan String -> Group -> Int -> IO ThreadId
reindeer q gp rid = forkIO $ forever $ do
  reindeer1 q gp rid
  randomDelay

santa :: TChan String -> Group -> Group -> Int -> IO ()
santa q eGroup rGroup i = do
  msg q $ show i ++ " --------------"
  (task, (inGate, outGate)) <- atomically $ 
    chooseGroup rGroup "deliver toys" <|> chooseGroup eGroup "meet in study"
  msg q $ "Ho! Ho! Ho! let's " ++ task ++ "!"
  operateGate inGate
  operateGate outGate
  where
    chooseGroup :: Group -> String -> STM (String, (Gate, Gate))
    chooseGroup gp task = do
      gates <- awaitGroup gp
      return (task, gates)
