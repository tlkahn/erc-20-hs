{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Erc20
  ( ERC20,
  )
where

import Control.Concurrent.STM
import Control.Monad (when)
import Data.Char
import qualified Data.HashTable.Class as C
import qualified Data.HashTable.IO as H
import qualified Data.Map as Map
import GHC.Generics
import Int (UIntN)

-- This needs to be updated to sync with the desired blockchain, e.g. ADA etc
type Address = TVar String

newtype Blockchain = Blockchain (Map.Map String Int) deriving (Show, Eq)

newtype BlockchainOp a = BlockchainOp (Blockchain -> (a, Blockchain))

runBlockchainOp :: BlockchainOp a -> Blockchain -> (a, Blockchain)
runBlockchainOp (BlockchainOp f) = f

depositOp :: String -> Int -> BlockchainOp ()
depositOp address amount = BlockchainOp depositHelper
  where
    depositHelper blockchain = ((), deposit address amount blockchain)

data ERC20 = ERC20
  { totalSupply :: UIntN 256,
    name :: String,
    decimals :: UIntN 8,
    symbol :: String
  }

class IERC20 a where
  balanceOf :: Address -> UIntN 256 -> IO ()
  transfer :: Address -> UIntN 256 -> IO ()
  transferFrom :: Address -> Address -> UIntN 256 -> IO ()

-- approve :: Address -> UIntN 256 -> IO()
-- allowance :: Address -> Address -> UIntN 256

instance IERC20 ERC20 where
  balanceOf addr = C.lookup . addr
  transfer from to amount =
    atomically
      ( do
          deposit to amount
          withdraw from amount
      )
  transfer to amount =
    atomically
      ( do
          depositOp to amount
      )
