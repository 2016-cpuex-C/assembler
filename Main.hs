{-# LANGUAGE MultiWayIf #-}

module Main where

import Types
import Parser hiding (Parser, (<.>))
import Inst

import Prelude              hiding (lex)
import Data.Word            (Word32)
import Data.Maybe           (fromMaybe)
import Data.Binary.Put      (putWord32be, runPut)
import Data.ByteString.Lazy (hPut)
import System.IO            (withFile, hPutStrLn, Handle, IOMode(..))
import Text.Printf          (printf)
import Control.Monad        (unless)
import System.FilePath
import Options.Applicative

main :: IO ()
main = execParser (info (helper <*> parseOpt) fullDesc) >>= \opts -> do
  let inputFile = infile opts
      outputBin = fromMaybe (inputFile -<.> "bin") (outfile opts)
      outputTxt = outputBin <.> "txt"
  parseResult <- parseAsm inputFile <$> readFile inputFile
  writeBin outputBin parseResult
  unless (noTxt opts) $ do
    let writer | h && l    = writeTxtHexLiteral
               | h         = writeTxtHex
               | l         = writeTxtBinLiteral
               | otherwise = writeTxtBin
               where h = inHex opts
                     l = literal opts
    writer outputTxt parseResult

-------------------------------------------------------------------------------
-- Writers
-------------------------------------------------------------------------------

writeTxtBinLiteral :: FilePath -> ParseResult -> IO ()
writeTxtBinLiteral = write fw iw
  where fw h x     = hPutStrLn h $ "32'b" ++ toStr (wordToBits32 x) ++ ","
        iw h (x,i) = hPutStrLn h $ "32'b" ++ toStr (wordToBits32 x) ++ ", // "++show i

writeTxtHexLiteral :: FilePath -> ParseResult -> IO ()
writeTxtHexLiteral = write fw iw
  where fw h x     = hPutStrLn h $ "8'0x" ++ toStr (wordToBits32 x) ++ ","
        iw h (x,i) = hPutStrLn h $ "8'0x" ++ toStr (wordToBits32 x) ++ ", // "++show i

writeTxtHex :: FilePath -> ParseResult -> IO ()
writeTxtHex = write fw iw
  where fw h x     = hPutStrLn h $ printf "%08lx" x
        iw h (x,_) = hPutStrLn h $ printf "%08lx" x

writeTxtBin :: FilePath -> ParseResult -> IO ()
writeTxtBin = write fw iw
  where fw h x     = hPutStrLn h $ toStr $ wordToBits32 x
        iw h (x,_) = hPutStrLn h $ toStr $ wordToBits32 x

writeBin :: FilePath -> ParseResult -> IO ()
writeBin = write fw iw
  where fw h x     = hPut h $ runPut $ putWord32be x
        iw h (x,_) = hPut h $ runPut $ putWord32be x

write :: (Handle -> Word32 -> IO ())
      -> (Handle -> (Word32, Inst) -> IO ())
      -> FilePath -> ParseResult -> IO ()
write fw iw out (ParseResult floats insts dicf dici) =
  withFile out WriteMode $ \h -> do
    mapM_ (fw h) floats
    fw h 0xffffffff -- 区切り文字
    mapM_ (iw h) $ [(decodeInst dici dicf i, i) | i <- insts]


-------------------------------------------------------------------------------
-- Commandline Options
-------------------------------------------------------------------------------

data CmdOpt = CmdOpt
              { outfile :: Maybe String
              , noTxt   :: Bool
              , inHex   :: Bool
              , literal :: Bool
              , infile  :: String
              }

parseOpt :: Parser CmdOpt
parseOpt = pure CmdOpt
  <*> option (Just <$> str)
    $$  short 'o'
    <=> metavar "OUTFILE"
    <=> help "outut file"
    <=> value Nothing
    <=> showDefaultWith (const "SRC:.s=.bin")
  <*> switch
    $$  long "no-txt"
    <=> help "do not write machine code in OUTFILE.txt in text format"
    <=> showDefault
  <*> switch
    $$  long "hex"
    <=> help "write OUTFILE.txt in hex digits"
    <=> showDefaultWith (const "bits")
  <*> switch
    $$  short 'l'
    <=> long "literal"
    <=> help ("write OUTFILE.txt in literal: `32'11...1,` or `8'0xffffffff,`")
    <=> showDefault
  <*> argument str (metavar "SRC")
  where
    infixr 7 $$
    infixr 8 <=>
    ($$) :: (a -> b) -> a -> b
    ($$) = ($)
    (<=>) :: Monoid m => m -> m -> m
    (<=>) = (<>)

