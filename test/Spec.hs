{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import ML
import Unit
import Test.Hspec

main :: IO ()
main = do
  hspec ML.spec
  hspec Unit.spec
