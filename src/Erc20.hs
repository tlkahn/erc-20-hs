{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Erc20
  ( ERC20,
    IERC20,
    balanceOf,
  )
where

type Address = String

data ERC20 = ERC20
  { totalSupply :: Int,
    name :: String,
    decimals :: Int,
    symbol :: String
  }

class IERC20 a where
  balanceOf :: ERC20 -> Address -> IO ()
  transfer :: ERC20 -> Address -> Int -> IO ()
  transferFrom :: ERC20 -> Address -> Address -> Int -> IO ()
  approve :: ERC20 -> Address -> Int -> IO ()
  allowance :: ERC20 -> Address -> Address -> Int

instance IERC20 ERC20 where
  balanceOf _ _ = pure ()
  transfer _ _ _ = pure ()
  transferFrom _ _ _ _ = pure ()
  approve _ _ _ = pure ()
  allowance _ _ _ = 0
