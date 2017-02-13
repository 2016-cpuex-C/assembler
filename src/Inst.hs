{-# LANGUAGE LambdaCase #-}

module Inst where

import           Types
import           Data.Word  (Word32)
import           Data.Map   (Map)
import qualified Data.Map   as M

decodeInst :: Map LabelI Word32 -> Map LabelF Word32 -> Inst -> Word32
decodeInst dici dicf = bitsToWord . instToBits dici dicf

labeliToBits :: Map LabelI Word32 -> LabelI -> Bits
labeliToBits dici li = paddingF 16 $ wordToBits n
  where n = M.findWithDefault (error $ show li ++ " not found") li dici

labelfToBits :: Map LabelF Word32 -> LabelF -> Bits
labelfToBits dicf lf = paddingF 16 $ wordToBits n
  where n = M.findWithDefault (error $ show lf ++ " not found") lf dicf

instToBits ::  Map LabelI Word32 -> Map LabelF Word32 -> Inst -> Bits
instToBits dici dicf e =
  let  liToBits' = labeliToBits dici
       lfToBits' = labelfToBits dicf
  in paddingB 32 . concat . (opcodeBits e:) $ case e of
    Sqrt    f1 f2       -> [toBits f1, toBits f2]
    Move    r1 r2       -> [toBits r1, toBits r2]
    Neg     r1 r2       -> [toBits r1, toBits r2]
    Add     r1 r2 r3    -> [toBits r1, toBits r2, toBits r3]
    Sub     r1 r2 r3    -> [toBits r1, toBits r2, toBits r3]
    Mult    r1 r2 r3    -> [toBits r1, toBits r2, toBits r3]
    Div     r1 r2 r3    -> [toBits r1, toBits r2, toBits r3]
    Addi    r1 r2 i     -> [toBits r1, toBits r2, toBits i]
    Multi   r1 r2 i     -> [toBits r1, toBits r2, toBits i]
    Divi    r1 r2 i     -> [toBits r1, toBits r2, toBits i]
    Movs    f1 f2       -> [toBits f1, toBits f2]
    Negs    f1 f2       -> [toBits f1, toBits f2]
    Adds    f1 f2 f3    -> [toBits f1, toBits f2, toBits f3]
    Subs    f1 f2 f3    -> [toBits f1, toBits f2, toBits f3]
    Muls    f1 f2 f3    -> [toBits f1, toBits f2, toBits f3]
    Divs    f1 f2 f3    -> [toBits f1, toBits f2, toBits f3]
    Srl     r1 r2 r3    -> [toBits r1, toBits r2, toBits r3]
    Sll     r1 r2 r3    -> [toBits r1, toBits r2, toBits r3]
    Srli    r1 r2 i     -> [toBits r1, toBits r2, toBits i]
    Slli    r1 r2 i     -> [toBits r1, toBits r2, toBits i]
    Li      r1 i        -> [toBits r1, toBits i]
    La      r1 li       -> [toBits r1, liToBits' li]
    Lwl     r1 lf       -> [toBits r1, lfToBits' lf]
    Lwr     r1 r2 i     -> [toBits r1, toBits r2, toBits i]
    Lsl     f1 lf       -> [toBits f1, lfToBits' lf]
    Lsr     f1 r2 i     -> [toBits f1, toBits r2, toBits i]
    Sw      r1 r2 i     -> [toBits r1, toBits r2, toBits i]
    Ss      f1 r2 i     -> [toBits f1, toBits r2, toBits i]
    Beq     r1 r2 li    -> [toBits r1, toBits r2, liToBits' li]
    Bne     r1 r2 li    -> [toBits r1, toBits r2, liToBits' li]
    Blt     r1 r2 li    -> [toBits r1, toBits r2, liToBits' li]
    Bgt     r1 r2 li    -> [toBits r1, toBits r2, liToBits' li]
    Beqi    r1 i  li    -> [toBits r1, toBits  i, liToBits' li]
    Bnei    r1 i  li    -> [toBits r1, toBits  i, liToBits' li]
    Blti    r1 i  li    -> [toBits r1, toBits  i, liToBits' li]
    Bgti    r1 i  li    -> [toBits r1, toBits  i, liToBits' li]
    Ceqs    f1 f2 li    -> [toBits f1, toBits f2, liToBits' li]
    Cles    f1 f2 li    -> [toBits f1, toBits f2, liToBits' li]
    Clts    f1 f2 li    -> [toBits f1, toBits f2, liToBits' li]
    J       li          -> [liToBits' li]
    Jal     li          -> [liToBits' li]
    Jr      r1          -> [toBits r1]
    Jalr    r1          -> [toBits r1]
    PrintC  r1          -> [toBits r1]
    ReadI   r1          -> [toBits r1]
    ReadF   f1          -> [toBits f1]
    And     r1 r2 r3    -> [toBits r1, toBits r2, toBits r3]
    Or      r1 r2 r3    -> [toBits r1, toBits r2, toBits r3]
    Xor     r1 r2 r3    -> [toBits r1, toBits r2, toBits r3]
    Andi    r1 r2 i     -> [toBits r1, toBits r2, toBits i]
    Ori     r1 r2 i     -> [toBits r1, toBits r2, toBits i]
    Xori    r1 r2 i     -> [toBits r1, toBits r2, toBits i]
    Swap    r1 r2       -> [toBits r1, toBits r2]
    Swaps   f1 f2       -> [toBits f1, toBits f2]
    Select  r1 r2 r3 r4 -> map toBits [r1,r2,r3,r4]
    Selects f1 r2 f3 f4 -> [toBits f1, toBits r2, toBits f3, toBits f4]
    Cmp     p  r1 r2 r3 -> map toBits [r1,r2,r3]             ++ [toBits p]
    Cmpi    p  r1 r2 i  -> map toBits [r1,r2] ++ [toBits i]  ++ [toBits p]
    Cmps    p  r1 f2 f3 -> toBits r1 : map toBits [f2,f3]    ++ [toBits p]
    Cvtsw   f  r        -> [toBits f, toBits r]
    Cvtws   r  f        -> [toBits r, toBits f]
    MAdds   f1 f2 f3 f4 -> map toBits [f1,f2,f3,f4]
    Exit                -> []

