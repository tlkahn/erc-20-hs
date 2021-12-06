module Main where

import Data.Map (Map, fromLis, lookup)

type Address = String

data ERC20 = ERC20
  { totalSupply :: Int,
    name :: String,
    decimals :: Int,
    symbol :: String,
    accounts :: [Address]
  }

data Blockchain = Blockchain Map Address Int

class IERC20 a where
  balanceOf :: a -> Address -> IO ()
  transfer :: a -> Address -> Int -> IO ()
  transferFrom :: a -> Address -> Address -> Int -> IO ()
  approve :: a -> Address -> Int -> IO ()
  allowance :: a -> Address -> Address -> Int

instance IERC20 ERC20 where
  balanceOf _ _ = do
    putStrLn $ show 100
  transfer _ _ _ = pure ()
  transferFrom _ _ _ _ = pure ()
  approve _ _ _ = pure ()
  allowance _ _ _ = 0

main :: IO ()
main = do
  addr <- "0x001"
  balance <- lookup addr blockchain
  print balance
  where
    erc20 = ERC20 1000 "Erc20" 18 "ERC20"
    blockchain = fromList [("0x001", 1000), ("0x002", 2000), ("0x003", 3000)]
