{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Unit where

import Common
import XFormat.Util

import Prelude hiding (lex, (^))
import Text.Printf
import Text.XFormat.Show

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

spec :: Spec
spec = do
  let equal1 f g' = equal1' f (showf g')
      equal1' f g x = monadicIO $ do
        y <- run $ sim $ g x
        assert $ y == f x

      equal2 f g' = equal2' f (showf g')
      equal2' f g x y = monadicIO $ do
        z <- run $ sim $ g x y
        assert $ z == f x y

      btoi :: Bool -> Int
      btoi b = if b then 1 else 0

  describe "sim fiszero" $
    it "is equal to \\x -> x==0.0" $ property $
      equal1 (\(x::Float)-> btoi $ x==0.0)
             ("if fiszero ("%ff%") then print_int 1 else print_int 0")
  describe "sim fispos" $
    it "is equal to \\x -> x>0.0" $ property $
      equal1 (\(x::Float)-> btoi $ x>0.0)
             ("if fispos ("%ff%") then print_int 1 else print_int 0")
  describe "sim fisneg" $
    it "is equal to \\x -> x<0.0" $ property $
      equal1 (\(x::Float)-> btoi $ x<0.0)
             ("if fisneg ("%ff%") then print_int 1 else print_int 0")
  describe "sim fless" $
    it "is equal to \\x y -> x < y" $ property $
      equal2 (\(x::Float) (y::Float) -> btoi $ x<y)
             ("if fless ("%ff%") ("%ff%") then print_int 1 else print_int 0")

  describe "sim float_of_int" $
    it "is equal to fromIntegral" $ property $
      equal1 (fromIntegral::Int->Float) ("print_float(float_of_int("%dd%"))")
  describe "sim int_of_float" $
    it "is equal to truncate" $ property $
      equal1 (truncate::Float->Int)  ("print_int(int_of_float("%ff%"))")
  describe "sim truncate" $
    it "is equal to truncate" $ property $
      equal1 (truncate::Float->Int) ("print_int(truncate("%ff%"))")
  describe "sim floor" $
    it "is equal to floor" $ property $
      equal1 floorFF ("print_float(floor("%ff%"))")

  describe "sim sin" $
    it "is equal to Prelude.sin" $ property $
      equal1 sin ("print_float(sin("%ff%"))")
  describe "sim cos" $
    it "is equal to Prelude.cos" $ property $
      equal1 cos ("print_float(cos("%ff%"))")
  describe "sim atan" $
    it "is equal to Prelude.atan" $ property $
      equal1 atan ("print_float(atan("%ff%"))")

  describe "sim fneg" $
    it "is equal to \\x->(-x)" $ property $
      equal1 (\x-> -x) ("print_float(fneg("%ff%"))")
  describe "sim fabs" $
    it "is equal to abs" $ property $
      equal1 abs ("print_float(fabs("%ff%"))")
  describe "sim fsqr" $
    it "is equal to \\x -> x * x" $ property $
      equal1 (\(x::Float) -> x * x) ("print_float(fsqr("%ff%"))")
  describe "sim fhalf" $
    it "is equal to \\x -> x * 0.5" $ property $
      equal1 (\(x::Float) -> x * 0.5) ("print_float(fhalf("%ff%"))")
  describe "sim sqrt" $
    it "is equal to sqrt" $ property $ \(Positive x) -> monadicIO $ do
        y <- run $ sim $ showf ("print_float(sqrt("%ff%"))") x
        assert $ y == sqrt x

  describe "sim (*)" $
    it "is equal to (*)" $ property $
      equal2 (*) ("print_int(("%dd%") * ("%dd%"))")

  describe "sim xor" $ do
    let w = printf "if xor %s %s then print_int 1 else print_int 0"
    it "is equal to xor" $ do
      sim (w "true"  "true" ) `shouldReturn` (0::Int)
      sim (w "true"  "false") `shouldReturn` (1::Int)
      sim (w "false" "true" ) `shouldReturn` (1::Int)
      sim (w "false" "false") `shouldReturn` (0::Int)
  {-
  -}

floorFF :: Float -> Float
floorFF = fromIntegral . (floor::Float->Int)

