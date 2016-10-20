{-# LANGUAGE LambdaCase #-}

module Inst where

import           Types
import           Data.Word  (Word32)
import           Data.Map   (Map)
import qualified Data.Map as M

decodeInst :: Map LabelI Word32 -> Map LabelF Word32 -> Inst -> Word32
decodeInst dici dicf = bitsToWord . instToBits dici dicf

-- 前を0埋め
paddingF :: Int -> Bits -> Bits
paddingF n s | length s > n = error $ "paddingF " ++ show n ++ " " ++ s
             | otherwise = replicate (n - length s) '0' ++ s
-- 後を0埋め
paddingB :: Int -> Bits -> Bits
paddingB n s | length s > n = error $ "padding " ++ show n ++ " " ++ s
             | otherwise = s ++ replicate (n - length s) '0'

opcodeBits :: Inst -> Bits
opcodeBits = paddingF 6 . wordToBits . opcode
regToBits  :: Reg -> Bits
regToBits (Reg i)   = paddingF 5 $ wordToBits i
fregToBits :: FReg -> Bits
fregToBits (FReg i) = paddingF 5 $ wordToBits i
immToBits  :: Imm -> Bits
immToBits (Imm i)   = paddingF 16 $ wordToBits i
labeliToBits :: Map LabelI Word32 -> LabelI -> Bits
labeliToBits dici li = paddingF 16 $ wordToBits n
  where n = M.findWithDefault (error $ show li ++ " not found") li dici
labelfToBits :: Map LabelF Word32 -> LabelF -> Bits
labelfToBits dicf lf = paddingF 16 $ wordToBits n
  where n = M.findWithDefault (error $ show lf ++ " not found") lf dicf

instToBits ::  Map LabelI Word32 -> Map LabelF Word32 -> Inst -> Bits
instToBits dici dicf e =
  let  labeliToBits' = labeliToBits dici
       labelfToBits' = labelfToBits dicf
  in paddingB 32 . concat . (opcodeBits e:) $ case e of
    Move   r1 r2    -> [regToBits  r1, regToBits  r2]
    Add    r1 r2 r3 -> [regToBits  r1, regToBits  r2, regToBits r3]
    Sub    r1 r2 r3 -> [regToBits  r1, regToBits  r2, regToBits r3]
    Mult   r1 r2 r3 -> [regToBits  r1, regToBits  r2, regToBits r3]
    Div    r1 r2 r3 -> [regToBits  r1, regToBits  r2, regToBits r3]
    Addi   r1 r2 i  -> [regToBits  r1, regToBits  r2, immToBits i]
    Subi   r1 r2 i  -> [regToBits  r1, regToBits  r2, immToBits i]
    Multi  r1 r2 i  -> [regToBits  r1, regToBits  r2, immToBits i]
    Divi   r1 r2 i  -> [regToBits  r1, regToBits  r2, immToBits i]
    Movs   f1 f2    -> [fregToBits f1, fregToBits f2]
    Adds   f1 f2 f3 -> [fregToBits f1, fregToBits f2, fregToBits f3]
    Subs   f1 f2 f3 -> [fregToBits f1, fregToBits f2, fregToBits f3]
    Muls   f1 f2 f3 -> [fregToBits f1, fregToBits f2, fregToBits f3]
    Divs   f1 f2 f3 -> [fregToBits f1, fregToBits f2, fregToBits f3]
    Srl    r1 r2 i  -> [regToBits  r1, regToBits  r2, immToBits i]
    Sll    r1 r2 i  -> [regToBits  r1, regToBits  r2, immToBits i]
    Li     r1 i     -> [regToBits  r1, immToBits i]
    La     r1 li    -> [regToBits  r1, labeliToBits' li]
    Lwl    r1 lf    -> [regToBits  r1, labelfToBits' lf]
    Lwr    r1 r2 i  -> [regToBits  r1, regToBits  r2, immToBits i]
    Lsl    f1 lf    -> [fregToBits f1, labelfToBits' lf]
    Lsr    f1 r2 i  -> [fregToBits f1, regToBits  r2, immToBits i]
    Sw     r1 r2 i  -> [regToBits  r1, regToBits  r2, immToBits i]
    Ss     f1 r2 i  -> [fregToBits f1, regToBits  r2, immToBits i]
    Beq    r1 r2 li -> [regToBits  r1, regToBits  r2, labeliToBits' li]
    Bne    r1 r2 li -> [regToBits  r1, regToBits  r2, labeliToBits' li]
    Blt    r1 r2 li -> [regToBits  r1, regToBits  r2, labeliToBits' li]
    Bgt    r1 r2 li -> [regToBits  r1, regToBits  r2, labeliToBits' li]
    Ceqs   f1 f2 li -> [fregToBits f1, fregToBits f2, labeliToBits' li]
    Cles   f1 f2 li -> [fregToBits f1, fregToBits f2, labeliToBits' li]
    Clts   f1 f2 li -> [fregToBits f1, fregToBits f2, labeliToBits' li]
    J      li       -> [labeliToBits' li]
    Jal    li       -> [labeliToBits' li]
    Jr     r1       -> [regToBits r1]
    Jalr   r1       -> [regToBits r1]
    PrintI r1       -> [regToBits r1]
    PrintF f1       -> [fregToBits f1]
    PrintC r1       -> [regToBits r1]
    ReadI  r1       -> [regToBits r1]
    ReadF  f1       -> [fregToBits f1]
    Sin    f1 f2    -> [fregToBits f1, fregToBits f2]
    Cos    f1 f2    -> [fregToBits f1, fregToBits f2]
    Atan   f1 f2    -> [fregToBits f1, fregToBits f2]
    Floor  f1 f2    -> [fregToBits f1, fregToBits f2]
    Sqrt   f1 f2    -> [fregToBits f1, fregToBits f2]
    Ftoi   r1 f2    -> [regToBits  r1, fregToBits f2]
    Itof   f1 r2    -> [fregToBits f1, regToBits  r2]
    Exit            -> []

