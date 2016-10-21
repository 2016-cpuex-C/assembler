
module Main where

import Types
import Lexer
import Parser
import Inst

import Prelude              hiding (lex)
import Data.Word            (Word32)
import Data.Binary.Put      (putWord32be, runPut)
import Data.ByteString.Lazy (hPut)
import System.IO            (withFile, hPutStrLn, Handle, IOMode(..))
import Text.Printf          (printf)
import Control.Monad        (when)
import Options.Applicative
import System.FilePath

main :: IO ()
main = execParser (info (helper <*> parseOpt) fullDesc) >>= \opts -> do
  let inputFile = infile opts
      outputBin = case outfile opts of
                    Nothing -> inputFile -<.> "bin"
                    Just o  -> o
      outputTxt = outputBin <.> "txt"
  parseResult <- parse . lex <$> readFile inputFile
  writeBin outputBin parseResult
  when (binTxt opts) $ writeTxt outputTxt parseResult

writeTxt :: FilePath -> ParseResult -> IO ()
writeTxt = write $ \h x -> hPutStrLn h (printf "0x%08lx" x)

---- 見たまんま変換するのでbig endian (compilerがleにすでに変換している)
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
              , binTxt  :: Bool
              , infile  :: String
              }

parseOpt :: Parser CmdOpt
parseOpt = pure CmdOpt
  <*> option (Just <$> str)
    $$  short 'o'
    <=> metavar "outfile"
    <=> help "outut file"
    <=> value Nothing
    <=> showDefaultWith (const "SRC:.s=.bin")
  <*> switch
    $$  short 't'
    <=> help "write machine code in `outfile.txt` in hex digits"
    <=> showDefault
  <*> (argument str (metavar "SRC"))
  where
    infixr 7 $$
    infixr 8 <=>
    ($$) :: (a -> b) -> a -> b
    ($$) = ($)
    (<=>) :: Monoid m => m -> m -> m
    (<=>) = (<>)

