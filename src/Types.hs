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
  Add   {} ->  2
  Addi  {} ->  3
  Sub   {} ->  4
  Subi  {} ->  5
  Mult  {} ->  6
  Multi {} ->  7
  Div   {} ->  8
  Divi  {} ->  9
  Movs  {} -> 10
  Adds  {} -> 11
  Subs  {} -> 12
  Muls  {} -> 13
  Divs  {} -> 14
  Srl   {} -> 15
  Sll   {} -> 16
  Li    {} -> 17
  La    {} -> 18
  Lwl   {} -> 19
  Lwr   {} -> 20
  Lsl   {} -> 21
  Lsr   {} -> 22
  Sw    {} -> 23
  Ss    {} -> 24
  Beq   {} -> 25
  Bne   {} -> 26
  Blt   {} -> 27
  Bgt   {} -> 28
  Ceqs  {} -> 29
  Cles  {} -> 30
  Clts  {} -> 31
  J     {} -> 32
  Jr    {} -> 33
  Jal   {} -> 34
  Jalr  {} -> 35
  PrintI{} -> 36
  PrintF{} -> 37
  PrintC{} -> 38
  ReadI {} -> 39
  ReadF {} -> 40
  Sin   {} -> 41
  Cos   {} -> 42
  Atan  {} -> 43
  Floor {} -> 44
  Sqrt  {} -> 45
  Ftoi  {} -> 46
  Itof  {} -> 47
  Exit  {} -> 48
  Neg   {} -> 49
  Negs  {} -> 50
  -- }}}

----------------
-- conversion --
----------------

-- # basic type # --

-- errorCheckしてないので注意 headで死にうる
bitsToWord :: Bits -> Word32
bitsToWord = fst . head . readInt 2 (`elem` "01") digitToInt

wordToBits :: Word32 -> Bits
wordToBits n = dropWhile (=='0') $ reverse [ if testBit n i then '1' else '0' | i <- [0..31]]

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
  where regs = M.fromList
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
            ]

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

