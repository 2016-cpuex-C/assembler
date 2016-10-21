{
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Lexer where

--import Types
--import Control.Lens
--import Control.Monad.State.Class
import Data.Word (Word32)
import Data.Int   (Int16)
}

%wrapper "monadUserState"

$digit  = 0-9
$hexdig = [0-9A-Fa-f]
$alpha  = [a-zA-Z]
@hex    = 0x $hexdig{8}
@ident  = $alpha ($alpha | $digit | \' | \_ | \.)*
@freg   = \$f $digit+
@reg    = \$ $alpha ($alpha | $digit | \' | \_ )*
@int   = $digit+
@comment = \#.*\n

tokens :-
  $white+   { skip                    }
  @comment  { skip                    }
  \,        { skip                    }
  ^@ident\: { mktk TokenLabelDef init }
  @freg     { mktk TokenFReg id       }
  @reg      { mktk TokenReg id        }
  @int      { mktk TokenInt read      }
  @hex      { mktk TokenHex read      }
  \-        { tk TokenNeg             }
  \(        { tk TokenLParen          }
  \)        { tk TokenRParen          }
  \:        { tk TokenColon           }
  \n        { tk TokenNL              }
  \.word    { tk TokenWord            }
  \.data    { tk TokenData            }
  \.text    { tk TokenText            }
  -- 命令   {{{
  mov       { tk MOV                  }
  neg       { tk NEG                  }
  move      { tk MOV                  }
  add       { tk ADD                  }
  addi      { tk ADDI                 }
  sub       { tk SUB                  }
  subi      { tk SUBI                 }
  mult      { tk MULT                 }
  multi     { tk MULTI                }
  div       { tk DIV                  }
  divi      { tk DIVI                 }
  mov\.s    { tk MOVS                 }
  neg\.s    { tk NEGS                 }
  add\.s    { tk ADDS                 }
  sub\.s    { tk SUBS                 }
  mul\.s    { tk MULS                 }
  div\.s    { tk DIVS                 }
  srl       { tk SRL                  }
  sll       { tk SLL                  }
  li        { tk LI                   }
  la        { tk LA                   }
  lwl       { tk LWL                  }
  lwr       { tk LWR                  }
  l\.sl     { tk LSL                  }
  l\.sr     { tk LSR                  }
  sw        { tk SW                   }
  s\.s      { tk SS                   }
  beq       { tk BEQ                  }
  bne       { tk BNE                  }
  blt       { tk BLT                  }
  bgt       { tk BGT                  }
  c\.eq\.s  { tk CEQS                 }
  c\.le\.s  { tk CLES                 }
  c\.lt\.s  { tk CLTS                 }
  j         { tk J_                   }
  jr        { tk JR                   }
  jal       { tk JAL                  }
  jalr      { tk JALR                 }
  print_i   { tk PRINTI               }
  print_f   { tk PRINTF               }
  print_c   { tk PRINTC               }
  read_i    { tk READI                }
  read_f    { tk READF                }
  sin       { tk SIN                  }
  cos       { tk COS                  }
  atan      { tk ATAN                 }
  floor     { tk FLOOR                }
  sqrt      { tk SQRT                 }
  ftoi      { tk FTOI                 }
  itof      { tk ITOF                 }
  exit      { tk EXIT                 }
  --        }}}
  @ident    { mktk TokenLabel id      }
  ^\.globl.*\n { skip }


{

data Token = TokenInt Int16 -- {{{
           | TokenHex Word32
           | TokenLabelDef String
           | TokenLabel String
           | TokenFReg String
           | TokenReg String
           | TokenComma
           | TokenColon
           | TokenLParen
           | TokenRParen
           | TokenEOF
           | TokenWord
           | TokenNL
           | TokenData
           | TokenText
           | TokenNeg
           | MOV
           | ADD
           | ADDI
           | SUB
           | SUBI
           | MULT
           | MULTI
           | DIV
           | DIVI
           | MOVS
           | ADDS
           | SUBS
           | MULS
           | DIVS
           | SRL
           | SLL
           | LI
           | LA
           | LWL
           | LWR
           | LSL
           | LSR
           | SW
           | SS
           | BEQ
           | BNE
           | BLT
           | BGT
           | CEQS
           | CLES
           | CLTS
           | J_
           | JAL
           | JR
           | JALR
           | PRINTI
           | PRINTF
           | PRINTC
           | READI
           | READF
           | SIN
           | COS
           | ATAN
           | FLOOR
           | SQRT
           | FTOI
           | ITOF
           | EXIT
           | NEG
           | NEGS
           deriving (Eq,Show) -- }}}

lex :: String -> [Token]
lex s = case runAlex s scanTokens of
  Right tks -> tks
  Left e -> error e

scanTokens :: Alex [Token]
scanTokens = loop []
  where loop acc = alexMonadScan >>= \case
            TokenEOF -> return $ reverse acc
            t -> loop $ t:acc

alexEOF :: Alex Token
alexEOF = return TokenEOF

tk :: Token -> AlexAction Token
tk t = token $ \_ _ -> t

mktk :: (a -> Token) -> (String -> a) -> AlexAction Token
mktk con read' = token $ \(_,_,_,s) n -> con $ read' $ take n s

-- commentまわり
newtype AlexUserState = AUS { depth :: Int }
alexInitUserState :: AlexUserState
alexInitUserState = AUS 0


}

