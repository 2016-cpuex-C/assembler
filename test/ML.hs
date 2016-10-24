{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module ML where

import Common

import           Prelude hiding (lex)
import           Control.Monad (forM_)
import           System.FilePath.Posix ((</>), (<.>))

import Test.Hspec

spec :: Spec
spec = describe "sim" $ forM_ targets $ \f -> do
  let ml  = "test" </> f <.> "ml"
      ans = "test" </> f <.> "ans"
  it ("passed " ++ ml) $ do
    x1 <- runML ml
    x2 <- readFile ans
    x1 `shouldBe` init x2 --最後の改行文字消す

targets :: [FilePath]
targets = [
    "ack"
  , "adder"
  , "cls-bug"
  , "cls-bug2"
  , "cls-rec"
  , "cls-reg-bug"
  , "even-odd"
  , "fib"
  , "funcomp"
--  , "gcd"
  , "inprod"
  , "inprod-rec"
  , "inprod-loop"
  , "join-reg"
  , "join-reg2"
  , "join-stack"
  , "join-stack2"
  , "join-stack3"
  , "print"
  , "shuffle"
  , "spill"
--  , "spill2"
  , "spill3"
  , "sum"
  , "sum-tail"
  , "non-tail-if"
--  , "non-tail-if2"
  , "matmul"
  , "matmul-flat"
  ]

