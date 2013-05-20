{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad
import Control.Exception
import Data.Typeable
import System.Random
import Prelude hiding (catch)

data Person = Person { name    :: String
                     , account :: TVar Int
                     , wallet  :: TVar Int
                     }

data World = World { people :: TVar [Person]
                   , total  :: TVar Int
                   }

transfer :: Person -> Person -> Int -> STM ()
transfer from to amount = do
  fromBal <- readTVar $ account from
  check $ fromBal >= amount
  toBal <- readTVar $ account to
  writeTVar (account from) (fromBal - amount)
  writeTVar (account to)   (toBal + amount)

deposit :: Person -> Int -> STM ()
deposit man amount = do
  pocket <- readTVar (wallet man)
  check $ pocket >= amount
  bal <- readTVar (account man)
  writeTVar (wallet  man) (pocket - amount)
  writeTVar (account man) (bal + amount)

takeOut :: Person -> Int -> STM ()
takeOut man amount = do
  bal <- readTVar (account man)
  check $ bal >= amount
  writeTVar (account man) (bal - amount)
  pocket <- readTVar (wallet man)
  writeTVar (wallet man) (pocket + amount)

move :: TVar Int -> TVar Int -> Int -> STM ()
move from to amount = do
  fromBal <- readTVar from
  check $ fromBal >= amount
  writeTVar from (fromBal - amount)
  toBal <- readTVar to
  writeTVar to (toBal + amount)

transfer' :: Person -> Person -> Int -> STM ()
transfer' from to amount = move (account from) (account to) amount

deposit' :: Person -> Int -> STM ()
deposit' man amount = move (wallet man) (account man) amount

takeOut' :: Person -> Int -> STM ()
takeOut' man amount = move (account man) (wallet man) amount

newPerson :: World -> String -> Int -> Int -> IO Person
newPerson world name bal pocket = atomically $ do
  person <- Person name <$> newTVar bal
                        <*> newTVar pocket
  w <- readTVar (people world)
  writeTVar (people world) (person : w)
  money <- readTVar (total world)
  writeTVar (total world) (money + bal + pocket)
  return person

moneyInvariant :: World -> STM Bool
moneyInvariant world = do
  ps  <- readTVar (people world)
  bals  <- sum <$> mapM (readTVar . account) ps
  pocks <- sum <$> mapM (readTVar . wallet) ps
  tot <- readTVar (total world)
  return $ tot == pocks + bals

moneyInvariant' :: World -> STM ()
moneyInvariant' world = do
  ps  <- readTVar (people world)
  bals  <- sum <$> mapM (readTVar . account) ps
  pocks <- sum <$> mapM (readTVar . wallet) ps
  tot <- readTVar (total world)
  unless (tot == pocks + bals) $ throwSTM TotalCurrencyAmountChanged

data BankInvariantException = TotalCurrencyAmountChanged
     deriving (Show, Eq, Typeable)
instance Exception BankInvariantException


newWorld :: IO World
newWorld = World <$> newTVarIO []
                 <*> newTVarIO 0

operate :: TChan String               -- ^ ログ用のキュー
        -> String                     -- ^ 操作の名前
        -> Person                     -- ^ 対象となる人
        -> (Person -> Int -> STM ())  -- ^ お金を移すトランザクション
        -> IO ()
operate q opName p op = handle (errorHandler q p opName) $ do
  put q $ name p ++ " is trying to " ++ opName
  amount <- randomRIO (1, 10000) -- ランダムに送金額を決定
  ans <- atomically $ optional $ do
    op p amount        -- 操作を行う．
    (,) <$> readTVar (account p) <*> readTVar (wallet p)
  case ans of
    Just (bal, pocket) ->
      put q $ concat [ name p, " ", opName, " $", show amount
                     , ", bal: ", show bal, " pocket: ", show pocket
                     ]
    Nothing ->
      put q $ name p ++ " doesn't have enough money to " ++ opName ++ " $" ++ show amount  

errorHandler :: TChan String -> Person -> String -> SomeException -> IO ()
errorHandler q p opName (SomeException e) = do
  put q $ name p ++ " failed to " ++ opName ++ " because " ++ show e

action :: TChan String -> World -> Person -> IO ThreadId
action q world p = forkIO $ forever $ do
  act <- randomRIO (0, 2) :: IO Int
  case act of
    0 -> operate q "deposit" p deposit
    1 -> operate q "take out" p takeOut
    2 -> handle (errorHandler q p "transfer") $ do
      put q $ name p ++ " is trying to transfer..."
      amount <- randomRIO (1, 10000)
      pss <- atomically $ filter ((/= name p) . name) <$> readTVar (people world)
      to  <- choice pss
      ans <- atomically $ optional $ do
        mWallet <- do transfer' p to amount
                      return Nothing
               <|> do bal <- readTVar (account p)
                      deposit p (amount - bal)
                      transfer p to amount
                      return $ Just (amount - bal)
        (,,,) mWallet <$> readTVar (account p) <*> readTVar (wallet p)
                      <*> readTVar (account to)
      case ans of
        Nothing ->
          put q $ "COULD NOT transfer $" ++ show amount ++ " from " ++ name p ++ " to " ++ name to
        Just (mHoge, fromBal, pock, toBal) -> do
          put q $ name p ++ " transfers $" ++ show amount ++ " to " ++ name to 
          case mHoge of
            Just bal -> put q $ name p ++ " has not enough money in account so deposit $" ++ show bal
            Nothing  -> return ()
          put q $ "now " ++ name p ++ " has $" ++ show fromBal ++ " in account, $" ++ show pock ++ " in wallet."
          put q $ "    " ++ name to ++ " has $" ++ show toBal ++ " in account."
    _ -> return ()
  put q ""
  threadDelay =<< randomRIO (1000, 100000)

put :: TChan a -> a -> IO ()
put q str = atomically $ writeTChan q str

choice :: [a] -> IO a
choice xs = do
  ix <- randomRIO (0, length xs - 1)
  return $ xs !! ix

main :: IO ()
main = do
  queue <- newTChanIO
  world <- newWorld
  atomically $ alwaysSucceeds $ moneyInvariant' world
  (john, yoko, paul) <- (,,) <$> newPerson world "John" 100000 5000
                             <*> newPerson world "Yoko" 10000  15000
                             <*> newPerson world "Paul" 500000 500
  initialMoney <- forM [john, yoko, paul] $ \p -> do
    (bal, pock) <- atomically $ (,) <$> readTVar (account p) <*> readTVar (wallet p)
    putStrLn $ name p ++ ": $" ++ show bal ++ " in account; $" ++ show pock ++ " in pocket."
    return (bal + pock)
  putStrLn $ "There are $" ++ show (sum initialMoney) ++ " in total"
  putStrLn "---------"
  tids <- mapM (action queue world) [john, yoko, paul]
  ltid <- forkIO $ forever $ atomically (readTChan queue) >>= putStrLn
  threadDelay $ 10^6
  mapM_ killThread tids
  atomically $ isEmptyTChan queue >>= check
  killThread ltid
  money <- forM [john, yoko, paul] $ \p -> atomically $
    (+) <$> readTVar (wallet p) <*> readTVar (account p)
  putStrLn $ "There are $" ++ show (sum money) ++ " in the end"
  

