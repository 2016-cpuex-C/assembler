{-# LANGUAGE LambdaCase #-}

module Types where

import           Data.Word (Word32)
import qualified Data.Map as M
import           Control.Exception.Base (assert)
import           Numeric    (readInt)
import           Data.Char  (digitToInt)
import           Data.Int   (Int16)
import           Data.Bits (testBit)

type Bits = String

newtype Reg    = Reg    Word32 deriving (Eq,Ord,Show)
newtype FReg   = FReg   Word32 deriving (Eq,Ord,Show)
newtype Imm    = Imm    Int16  deriving (Eq,Ord,Show) -- Int16
newtype LabelF = LabelF String deriving (Eq,Ord,Show)
newtype LabelI = LabelI String deriving (Eq,Ord,Show)

data Inst = Move   Reg    Reg -- {{{
          | Add    Reg    Reg    Reg
          | Addi   Reg    Reg    Imm
          | Sub    Reg    Reg    Reg
          | Subi   Reg    Reg    Imm
          | Mult   Reg    Reg    Reg
          | Multi  Reg    Reg    Imm
          | Div    Reg    Reg    Reg
          | Divi   Reg    Reg    Imm
          | Movs   FReg   FReg
          | Adds   FReg   FReg   FReg
          | Subs   FReg   FReg   FReg
          | Muls   FReg   FReg   FReg
          | Divs   FReg   FReg   FReg
          | Srl    Reg    Reg    Imm
          | Sll    Reg    Reg    Imm
          | Li     Reg    Imm
          | La     Reg    LabelI
          | Lwl    Reg    LabelF
          | Lwr    Reg    Reg    Imm
          | Lsl    FReg   LabelF
          | Lsr    FReg   Reg    Imm
          | Sw     Reg    Reg    Imm
          | Ss     FReg   Reg    Imm
          | Beq    Reg    Reg    LabelI
          | Bne    Reg    Reg    LabelI
          | Blt    Reg    Reg    LabelI
          | Bgt    Reg    Reg    LabelI
          | Ceqs   FReg   FReg   LabelI
          | Cles   FReg   FReg   LabelI
          | Clts   FReg   FReg   LabelI
          | Sin    FReg   FReg
          | Cos    FReg   FReg
          | Atan   FReg   FReg
          | Floor  FReg   FReg
          | Sqrt   FReg   FReg
          | Ftoi   Reg    FReg
          | Itof   FReg   Reg
          | J      LabelI
          | Jal    LabelI
          | Jr     Reg
          | Jalr   Reg
          | PrintI Reg
          | PrintF FReg
          | PrintC Reg
          | PrintB Reg
          | ReadI  Reg
          | ReadF  FReg
          | Exit
          | Neg    Reg    Reg
          | Negs   FReg   FReg
          deriving (Eq, Ord, Show)-- }}}

------------
-- opcode --
------------

opcode :: Inst -> Word32
opcode = \case -- {{{
  Move  {} ->  1
  Neg   {} ->  2
  Add   {} ->  3
  Addi  {} ->  4
  Sub   {} ->  5
  Subi  {} ->  6
  Mult  {} ->  7
  Multi {} ->  8
  Div   {} ->  9
  Divi  {} -> 10
  Movs  {} -> 11
  Negs  {} -> 12
  Adds  {} -> 13
  Subs  {} -> 14
  Muls  {} -> 15
  Divs  {} -> 16
  Srl   {} -> 17
  Sll   {} -> 18
  Li    {} -> 19
  La    {} -> 20
  Lwl   {} -> 21
  Lwr   {} -> 22
  Lsl   {} -> 23
  Lsr   {} -> 24
  Sw    {} -> 25
  Ss    {} -> 26
  Beq   {} -> 27
  Bne   {} -> 28
  Blt   {} -> 29
  Bgt   {} -> 30
  Ceqs  {} -> 31
  Cles  {} -> 32
  Clts  {} -> 33
  J     {} -> 34
  Jr    {} -> 35
  Jal   {} -> 36
  Jalr  {} -> 37
  PrintI{} -> 38
  PrintF{} -> 39
  PrintC{} -> 40
  ReadI {} -> 41
  ReadF {} -> 42
  Sin   {} -> 43
  Cos   {} -> 44
  Atan  {} -> 45
  Floor {} -> 46
  Sqrt  {} -> 47
  Ftoi  {} -> 48
  Itof  {} -> 49
  Exit  {} -> 50
  PrintB{} -> 51
  -- }}}

----------------
-- conversion --
----------------

-- # basic type # --

-- errorCheckしてないので注意 headで死にうる
bitsToWord :: Bits -> Word32
bitsToWord = fst . head . readInt 2 (`elem` "01") digitToInt

wordToBits :: Word32 -> Bits
wordToBits = dropWhile (=='0') . wordToBits32
wordToBits32 :: Word32 -> Bits
wordToBits32 n = reverse [ if testBit n i then '1' else '0' | i <- [0..31]]

int16ToBits :: Int16 -> Bits
int16ToBits n = reverse [ if testBit n i then '1' else '0' | i <- [0..15]]

-- # data type -> Bits # --

immToBits :: Imm -> Bits
immToBits (Imm n) = int16ToBits n

opcodeBits :: Inst -> Bits
opcodeBits = paddingF 6 . wordToBits . opcode

regToBits  :: Reg -> Bits
regToBits (Reg i)   = paddingF 5 $ wordToBits i

fregToBits :: FReg -> Bits
fregToBits (FReg i) = paddingF 5 $ wordToBits i


-- # register # --

strToFReg :: String -> FReg
strToFReg s = assert (take 2 s == "$f") $ FReg (read $ drop 2 s)

strToReg :: String -> Reg
strToReg s = case M.lookup s regs of
               Nothing -> error $ "unknown register " ++ s
               Just i  -> Reg i
  where regs = M.fromList --{{{
            [ ("$zero",0)
            , ("$at",  1)
            , ("$v0",  2)
            , ("$v1",  3)
            , ("$a0",  4)
            , ("$a1",  5)
            , ("$a2",  6)
            , ("$a3",  7)
            , ("$t0",  8)
            , ("$t1",  9)
            , ("$t2", 10)
            , ("$t3", 11)
            , ("$t4", 12)
            , ("$t5", 13)
            , ("$t6", 14)
            , ("$t7", 15)
            , ("$s0", 16)
            , ("$s1", 17)
            , ("$s2", 18)
            , ("$s3", 19)
            , ("$s4", 20)
            , ("$s5", 21)
            , ("$s6", 22)
            , ("$s7", 23)
            , ("$t8", 24)
            , ("$t9", 25)
            , ("$k0", 26)
            , ("$k1", 27)
            , ("$gp", 28)
            , ("$sp", 29)
            , ("$fp", 30)
            , ("$ra", 31)
            ] --}}}

----------
-- Util --
----------
-- 前を0埋め
paddingF :: Int -> Bits -> Bits
paddingF n s | length s > n = error $ "paddingF " ++ show n ++ " " ++ s
             | otherwise = replicate (n - length s) '0' ++ s
-- 後を0埋め
paddingB :: Int -> Bits -> Bits
paddingB n s | length s > n = error $ "padding " ++ show n ++ " " ++ s
             | otherwise = s ++ replicate (n - length s) '0'

