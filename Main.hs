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

writeTxtBin :: FilePath -> ParseResult -> IO ()
writeTxtBin = write $ \h x -> hPutStrLn h (wordToBits32 x)

writeTxtHex :: FilePath -> ParseResult -> IO ()
writeTxtHex = write $ \h x -> hPutStrLn h (printf "%08lx" x)

writeTxtBinLiteral :: FilePath -> ParseResult -> IO ()
writeTxtBinLiteral = write $ \h x -> hPutStrLn h ("32'b"++wordToBits32 x++",")

writeTxtHexLiteral :: FilePath -> ParseResult -> IO ()
writeTxtHexLiteral = write $ \h x -> hPutStrLn h ("8'0x"++wordToBits32 x++",")

writeBin :: FilePath -> ParseResult -> IO ()
writeBin = write $ \h x -> hPut h (runPut $ putWord32be x)

write :: (Handle -> Word32 -> IO ()) -> FilePath -> ParseResult -> IO ()
write w out (ParseResult floats insts dicf dici) =
  withFile out WriteMode $ \h ->
    mapM_ (w h) $ concat
      [ floats
      , [0xffffffff]
      , map (decodeInst dici dicf) insts]

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

