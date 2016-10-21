{
module Parser where

import           Types
import           Lexer
import           Control.Lens
import           Control.Monad.State
import           Data.Word  (Word32)
import           Data.Map   (Map)
import qualified Data.Map as M
}

%name parse_
%tokentype { Token }
%monad { State S } { (>>=) } { return }
%error { parseError }

%token
     int    { TokenInt $$      }
     hex    { TokenHex $$      }
     l      { TokenLabel $$    }
     ldef   { TokenLabelDef $$ }
     reg    { TokenReg $$      }
     freg   { TokenFReg $$     }
     ','    { TokenComma       }
     '('    { TokenLParen      }
     ')'    { TokenRParen      }
     '-'    { TokenNeg         }
     eof    { TokenEOF         }
     data   { TokenData        }
     text   { TokenText        }
     word   { TokenWord        }
     mov    { MOV              }
     neg    { NEG              }
     add    { ADD              }
     addi   { ADDI             }
     sub    { SUB              }
     subi   { SUBI             }
     mult   { MULT             }
     multi  { MULTI            }
     div    { DIV              }
     divi   { DIVI             }
     movs   { MOVS             }
     negs   { NEGS             }
     adds   { ADDS             }
     subs   { SUBS             }
     muls   { MULS             }
     divs   { DIVS             }
     srl    { SRL              }
     sll    { SLL              }
     li     { LI               }
     la     { LA               }
     lwl    { LWL              }
     lwr    { LWR              }
     lsl    { LSL              }
     lsr    { LSR              }
     sw     { SW               }
     ss     { SS               }
     beq    { BEQ              }
     bne    { BNE              }
     blt    { BLT              }
     bgt    { BGT              }
     ceqs   { CEQS             }
     cles   { CLES             }
     clts   { CLTS             }
     j      { J_               }
     jal    { JAL              }
     jr     { JR               }
     jalr   { JALR             }
     printi { PRINTI           }
     printf { PRINTF           }
     printc { PRINTC           }
     readi  { READI            }
     readf  { READF            }
     sin    { SIN              }
     cos    { COS              }
     atan   { ATAN             }
     floor  { FLOOR            }
     sqrt   { SQRT             }
     ftoi   { FTOI             }
     itof   { ITOF             }
     exit   { EXIT             }

%%

Main :: { ([Word32],[Inst]) }
    : data Data text Text { ($2, $4) }

Data :: { [Word32] }
    : Datum Data { $1:$2 }
    |            { []    }

Datum :: { Word32 }
    : LabelFDef word hex {% addLabelF $1 >> return $3 }

Text :: { [Inst] }
    : Block Text { $1++$2 }
    | Block      { $1     }

Block :: { [Inst] }
    : LabelIDef Insts {% addLabelI $1
                         >> instCnt += fromIntegral (length $2)
                         >> return $2 }

Insts :: { [Inst] }
    : Inst Insts { $1:$2 }
    |            { []  }

Inst :: { Inst }
    : mov    Reg    Reg           { Move   $2 $3    }
    | neg    Reg    Reg           { Neg    $2 $3    }
    | add    Reg    Reg    Reg    { Add    $2 $3 $4 }
    | addi   Reg    Reg    Imm    { Addi   $2 $3 $4 }
    | sub    Reg    Reg    Reg    { Sub    $2 $3 $4 }
    | subi   Reg    Reg    Imm    { Subi   $2 $3 $4 }
    | mult   Reg    Reg    Reg    { Mult   $2 $3 $4 }
    | multi  Reg    Reg    Imm    { Multi  $2 $3 $4 }
    | div    Reg    Reg    Reg    { Div    $2 $3 $4 }
    | divi   Reg    Reg    Imm    { Divi   $2 $3 $4 }
    | movs   FReg   FReg          { Movs   $2 $3    }
    | negs   FReg   FReg          { Negs   $2 $3    }
    | adds   FReg   FReg   FReg   { Adds   $2 $3 $4 }
    | subs   FReg   FReg   FReg   { Subs   $2 $3 $4 }
    | muls   FReg   FReg   FReg   { Muls   $2 $3 $4 }
    | divs   FReg   FReg   FReg   { Divs   $2 $3 $4 }
    | srl    Reg    Reg    Imm    { Srl    $2 $3 $4 }
    | sll    Reg    Reg    Imm    { Sll    $2 $3 $4 }
    | li     Reg    Imm           { Li     $2 $3    }
    | la     Reg    LabelI        { La     $2 $3    }
    | lwl    Reg    LabelF        { Lwl    $2 $3    }
    | lwr    Reg    Reg    Imm    { Lwr    $2 $3 $4 }
    | lsl    FReg   LabelF        { Lsl    $2 $3    }
    | lsr    FReg   Reg    Imm    { Lsr    $2 $3 $4 }
    | sw     Reg    Reg    Imm    { Sw     $2 $3 $4 }
    | ss     FReg   Reg    Imm    { Ss     $2 $3 $4 }
    | beq    Reg    Reg    LabelI { Beq    $2 $3 $4 }
    | bne    Reg    Reg    LabelI { Bne    $2 $3 $4 }
    | blt    Reg    Reg    LabelI { Blt    $2 $3 $4 }
    | bgt    Reg    Reg    LabelI { Bgt    $2 $3 $4 }
    | ceqs   FReg   FReg   LabelI { Ceqs   $2 $3 $4 }
    | cles   FReg   FReg   LabelI { Cles   $2 $3 $4 }
    | clts   FReg   FReg   LabelI { Clts   $2 $3 $4 }
    | sin    FReg   FReg          { Sin    $2 $3    }
    | cos    FReg   FReg          { Cos    $2 $3    }
    | atan   FReg   FReg          { Atan   $2 $3    }
    | floor  FReg   FReg          { Floor  $2 $3    }
    | sqrt   FReg   FReg          { Sqrt   $2 $3    }
    | ftoi   Reg    FReg          { Ftoi   $2 $3    }
    | itof   FReg   Reg           { Itof   $2 $3    }
    | j      LabelI               { J      $2       }
    | jal    LabelI               { Jal    $2       }
    | jr     Reg                  { Jr     $2       }
    | jalr   Reg                  { Jalr   $2       }
    | printi Reg                  { PrintI $2       }
    | printf FReg                 { PrintF $2       }
    | printc Reg                  { PrintC $2       }
    | readi  Reg                  { ReadI  $2       }
    | readf  FReg                 { ReadF  $2       }
    | exit                        { Exit            }
    -- これはcompilerでもやるべき
    | addi   Reg Reg '-' Imm { Subi $2 $3 $5 }
    | subi   Reg Reg '-' Imm { Addi $2 $3 $5 }
    | lwr Reg  Imm '(' Reg ')' { Lwr    $2 $5 $3      }
    | lsr FReg Imm '(' Reg ')' { Lsr    $2 $5 $3      }
    | sw  Reg  Imm '(' Reg ')' { Sw     $2 $5 $3      }
    | ss  FReg Imm '(' Reg ')' { Ss     $2 $5 $3      }
    | lwr Reg      '(' Reg ')' { Lwr    $2 $4 (Imm 0) }
    | lsr FReg     '(' Reg ')' { Lsr    $2 $4 (Imm 0) }
    | sw  Reg      '(' Reg ')' { Sw     $2 $4 (Imm 0) }
    | ss  FReg     '(' Reg ')' { Ss     $2 $4 (Imm 0) }


LabelFDef :: { LabelF } : ldef      { LabelF $1    }
LabelIDef :: { LabelI } : ldef      { LabelI $1    }
LabelF    :: { LabelF } : l         { LabelF $1    }
LabelI    :: { LabelI } : l         { LabelI $1    }
Reg       :: { Reg    } : reg       { strToReg $1  }
FReg      :: { FReg   } : freg      { strToFReg $1 }
Imm       :: { Imm    } : int       { Imm $1       }
                        | '-' int   { Imm (-$2)    }

{

data ParseResult = ParseResult [Word32] [Inst] (Map LabelF Word32) (Map LabelI Word32)
    deriving Show

parse :: [Token] -> ParseResult
parse tks =
    let ((hs,is),s) = runState (parse_ tks) (S M.empty M.empty (-1) 0)
    in  ParseResult hs is (s^.floatMap) (s^.instMap)


data S = S { _floatMap    :: Map LabelF Word32
           , _instMap     :: Map LabelI Word32
           , _floatCnt    :: Word32
           , _instCnt     :: Word32
           } deriving (Eq,Ord,Show)

-- makeLenses使えないっぽい
floatMap :: Lens' S (Map LabelF Word32)
instMap  :: Lens' S (Map LabelI Word32)
floatCnt :: Lens' S Word32
instCnt  :: Lens' S Word32
floatMap = lens _floatMap $ \s x -> s { _floatMap = x }
instMap  = lens _instMap  $ \s x -> s { _instMap  = x }
floatCnt = lens _floatCnt $ \s x -> s { _floatCnt = x }
instCnt  = lens _instCnt  $ \s x -> s { _instCnt  = x }

addLabelF :: LabelF -> State S ()
addLabelF lf = modifying floatMap . M.insert lf =<< floatCnt <+= 1
addLabelI :: LabelI -> State S ()
addLabelI li = modifying instMap . M.insert li =<< use instCnt

parseError :: [Token] -> State S a
parseError tks = error $ "Parse Error: remaining tokens are\n" ++ show (take 10 tks) ++ "..."

}


