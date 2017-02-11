{-# LANGUAGE LambdaCase #-}

module Types where

import           Prelude hiding (Ordering(..))
import           Data.Word (Word32)
import qualified Data.Map as M
import           Control.Exception.Base (assert)
import           Numeric    (readInt)
import           Data.Char  (digitToInt)
import           Data.Int   (Int16)
import           Data.Bits (testBit)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

--type Bits = String
type Bits = [Bit]
data Bit = I | O
  deriving (Show,Eq,Ord)

toStr :: Bits -> String
toStr = map $ \case
  I -> '1'
  O -> '0'

newtype Reg    = Reg    Word32 deriving (Eq,Ord,Show)
newtype FReg   = FReg   Word32 deriving (Eq,Ord,Show)
newtype Imm    = Imm    Int16  deriving (Eq,Ord,Show)
newtype Imm5   = Imm5   Int16  deriving (Eq,Ord,Show)
newtype LabelF = LabelF String deriving (Eq,Ord,Show)
newtype LabelI = LabelI String deriving (Eq,Ord,Show)
data Predicate = EQ | NE | LE | GE | LT | GT
               deriving (Show,Eq,Ord,Enum)

data Inst -- {{{
  = Move      Reg     Reg
  | Sqrt      FReg    FReg
  | Neg       Reg     Reg
  | Add       Reg     Reg     Reg
  | Addi      Reg     Reg     Imm
  | Sub       Reg     Reg     Reg
  | Mult      Reg     Reg     Reg
  | Multi     Reg     Reg     Imm
  | Div       Reg     Reg     Reg
  | Divi      Reg     Reg     Imm
  | Movs      FReg    FReg
  | Negs      FReg    FReg
  | Adds      FReg    FReg    FReg
  | Subs      FReg    FReg    FReg
  | Muls      FReg    FReg    FReg
  | Divs      FReg    FReg    FReg
  | Srli      Reg     Reg     Imm
  | Slli      Reg     Reg     Imm
  | Srl       Reg     Reg     Reg
  | Sll       Reg     Reg     Reg
  | Li        Reg     Imm
  | La        Reg     LabelI
  | Lwl       Reg     LabelF
  | Lwr       Reg     Reg     Imm
  | Lsl       FReg    LabelF
  | Lsr       FReg    Reg     Imm
  | Sw        Reg     Reg     Imm
  | Ss        FReg    Reg     Imm
  | Beq       Reg     Reg     LabelI
  | Bne       Reg     Reg     LabelI
  | Blt       Reg     Reg     LabelI
  | Bgt       Reg     Reg     LabelI
  | Beqi      Reg     Imm5    LabelI
  | Bnei      Reg     Imm5    LabelI
  | Blti      Reg     Imm5    LabelI
  | Bgti      Reg     Imm5    LabelI
  | Ceqs      FReg    FReg    LabelI
  | Cles      FReg    FReg    LabelI
  | Clts      FReg    FReg    LabelI
  | J         LabelI
  | Jr        Reg
  | Jal       LabelI
  | Jalr      Reg
  | PrintC    Reg
  | ReadI     Reg
  | ReadF     FReg
  | And       Reg     Reg     Reg
  | Or        Reg     Reg     Reg
  | Xor       Reg     Reg     Reg
  | Andi      Reg     Reg     Imm
  | Ori       Reg     Reg     Imm
  | Xori      Reg     Reg     Imm
  | Swap      Reg     Reg
  | Swaps     FReg    FReg
  | Select    Reg     Reg     Reg     Reg
  | Selects   FReg    Reg     FReg    FReg
  | Cmp       Predicate   Reg     Reg     Reg
  | Cmpi      Predicate   Reg     Reg     Imm5
  | Cmps      Predicate   Reg     FReg    FReg
  | Cvtsw     FReg    Reg
  | Cvtws     Reg     FReg
  | MAdds     FReg    FReg    FReg    FReg
  | Exit
  deriving (Eq, Ord, Show)-- }}}

class ToBits a where
  toBits :: a -> Bits

instance ToBits Reg where
  toBits = regToBits
instance ToBits FReg where
  toBits = fregToBits
instance ToBits Imm where
  toBits = immToBits
instance ToBits Imm5 where
  toBits = imm5ToBits
instance ToBits Predicate where
  toBits = predToBits

------------
-- opcode --
------------

opcode :: Inst -> Word32
opcode = \case -- {{{
  Sqrt   {} ->  0
  Move   {} ->  1
  Neg    {} ->  2
  Add    {} ->  3
  Addi   {} ->  4
  Sub    {} ->  5
  --Subi   {} ->  6
  Mult   {} ->  7
  Multi  {} ->  8
  Div    {} ->  9
  Divi   {} -> 10
  Movs   {} -> 11
  Negs   {} -> 12
  Adds   {} -> 13
  Subs   {} -> 14
  Muls   {} -> 15
  Divs   {} -> 16
  Srl    {} -> 17
  Sll    {} -> 18
  Li     {} -> 19
  La     {} -> 20
  Lwl    {} -> 21
  Lwr    {} -> 22
  Lsl    {} -> 23
  Lsr    {} -> 24
  Sw     {} -> 25
  Ss     {} -> 26
  Beq    {} -> 27
  Bne    {} -> 28
  Blt    {} -> 29
  Bgt    {} -> 30
  Ceqs   {} -> 31
  Cles   {} -> 32
  Clts   {} -> 33
  J      {} -> 34
  Jr     {} -> 35
  Jal    {} -> 36
  Jalr   {} -> 37
  --PrintI {} -> 38
  --PrintF {} -> 39
  Srli   {} -> 38
  Slli   {} -> 39
  PrintC {} -> 40
  ReadI  {} -> 41
  ReadF  {} -> 42
  Beqi   {} -> 43
  And    {} -> 44
  Or     {} -> 45
  Xor    {} -> 46
  Andi   {} -> 47
  Ori    {} -> 48
  Xori   {} -> 49
  Exit   {} -> 50
  Swap   {} -> 51
  Swaps  {} -> 52
  Select {} -> 53
  Selects{} -> 54
  Cmp    {} -> 55
  Cmpi   {} -> 56
  Cmps   {} -> 57
  Cvtsw  {} -> 58
  Cvtws  {} -> 59
  MAdds  {} -> 60
  Bnei   {} -> 61
  Blti   {} -> 62
  Bgti   {} -> 63
  -- }}}

-------------------------------------------------------------------------------
-- Conversion
-------------------------------------------------------------------------------

----------------
-- basic type --
----------------

bitsToWord :: Bits -> Word32
bitsToWord = fst . head . readInt 2 (`elem` "01") digitToInt . toStr

wordToBits :: Word32 -> Bits
wordToBits = dropWhile (==O) . wordToBits32

wordToBits32 :: Word32 -> Bits
wordToBits32 n = reverse [ if testBit n i then I else O | i <- [0..31] ]

int16ToBits :: Int16 -> Bits
int16ToBits n = reverse [ if testBit n i then I else O | i <- [0..15] ]

int16To5Bits :: Int16 -> Bits
int16To5Bits n = reverse [ if testBit n i then I else O | i <- [0..4] ]

------------------
-- data -> Bits --
------------------

immToBits :: Imm -> Bits
immToBits (Imm n) = int16ToBits n

imm5ToBits :: Imm5 -> Bits
imm5ToBits (Imm5 n) = int16To5Bits n

opcodeBits :: Inst -> Bits
opcodeBits = paddingF 6 . wordToBits . opcode

regToBits  :: Reg -> Bits
regToBits (Reg i)   = paddingF 5 $ wordToBits i

fregToBits :: FReg -> Bits
fregToBits (FReg i) = paddingF 5 $ wordToBits i

predToBits :: Predicate -> Bits
predToBits = \case
  EQ -> [O,O,O]
  NE -> [O,O,I]
  LE -> [O,I,O]
  GE -> [O,I,I]
  LT -> [I,O,O]
  GT -> [I,O,I]

--------------
-- register --
--------------

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

-------------------------------------------------------------------------------
-- Util
-------------------------------------------------------------------------------

-- 前を0埋め
paddingF :: Int -> Bits -> Bits
paddingF n s | length s > n = error $ "paddingF " ++ show n ++ " " ++ show s
             | otherwise = replicate (n - length s) O ++ s

-- 後を0埋め
paddingB :: Int -> Bits -> Bits
paddingB n s | length s > n = error $ "paddingB " ++ show n ++ " " ++ show s
             | otherwise = s ++ replicate (n - length s) O

