{-# LANGUAGE LambdaCase #-}

module Common where

import Types
import Parser
import Inst

import Prelude        hiding (lex)
import System.IO             (withFile, hPutStrLn, IOMode(..))
import System.Process
import System.FilePath.Posix ((-<.>))

assemble :: FilePath -> IO ()
assemble infile = do
  let outfile = infile -<.> "bin.txt"
  (ParseResult floats insts dicf dici) <- parseAsm infile <$> readFile infile
  withFile outfile WriteMode $ \h ->
    mapM_ (\x -> hPutStrLn h (wordToBits32 x)) $ concat
      [floats, [0xffffffff], map (decodeInst dici dicf) insts]

runSim :: FilePath -> IO String
runSim bintxt = readProcess "../binarysimulator/bsim" [bintxt] ""

compile :: FilePath -> IO ()
compile ml = callProcess "../compiler/min-caml" ["--log", "/dev/null", ml]

--------
--------

sim :: (Read a) => String -> IO a
sim str = do
  writeFile tmpML str
  read <$> runML tmpML

runML :: FilePath -> IO String
runML ml = compile ml >> runS (ml -<.> ".s")

runS :: FilePath -> IO String
runS s = assemble s >> runSim (s -<.> "bin.txt")

tmpBinTxt :: FilePath
tmpBinTxt = tmpS -<.> "bin.txt"

tmpS :: FilePath
tmpS = tmpS -<.> "s"

tmpML :: FilePath
tmpML = "test/tmp/min-caml-hs.ml"

