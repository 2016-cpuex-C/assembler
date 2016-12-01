{-# LANGUAGE TemplateHaskell #-}

module Parser where

import           Types

import           Data.Int (Int16)
import           Data.Word (Word32)
import           Control.Lens hiding ((<.>))
import           Control.Monad (void)
import           Data.Map (Map)
import qualified Data.Map as M
import           Text.Parsec
import qualified Text.Parsec.Token as P
import           Text.Parsec.Language (haskellDef)

-------------------------------------------------------------------------------
-- Data Types
-------------------------------------------------------------------------------

type Parser = ParsecT String S Identity

data ParseResult
  = ParseResult [Word32] [Inst] (Map LabelF Word32) (Map LabelI Word32)
  deriving Show

data S = S { _floatMap    :: Map LabelF Word32
           , _instMap     :: Map LabelI Word32
           , _floatCnt    :: Word32
           , _instCnt     :: Word32
           } deriving (Eq,Ord,Show)
makeLenses ''S

initS :: S
initS = S M.empty M.empty 0 0

incInstCnt :: Parser ()
incInstCnt = modifyState $ over instCnt succ

incFloatCnt :: Parser ()
incFloatCnt = modifyState $ over floatCnt succ

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

parseAsm :: FilePath -> String -> ParseResult
parseAsm f s = case runParser mainP initS f s of
                 Right r -> r
                 Left e -> error $ show e

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

mainP :: Parser ParseResult
mainP = do
  whiteSpace
  void $ symbol ".data"
  xs <- many datum
  void $ symbol ".text"
  void $ symbol ".globl"
  void $ symbol "main"
  is <- concat <$> many block
  s  <- getState
  return $ ParseResult xs is (s^.floatMap) (s^.instMap)

datum :: Parser Word32
datum = labelFDef >>= addLabelF >> symbol ".word" >> incFloatCnt >> hex
  where addLabelF lf = do
          fcnt <- view floatCnt <$> getState
          modifyState $ over floatMap (M.insert lf fcnt)

block :: Parser [Inst]
block = labelIDef >>= addLabelI >> many inst
  where addLabelI li = do
          s <- getState
          let icnt = view instCnt s
              fcnt = view floatCnt s
          modifyState $ over instMap (M.insert li (icnt+fcnt+1))

inst :: Parser Inst
inst = incInstCnt >> choice [
      try (symbol "move"   ) >> Move   <$> reg  <.> reg
    , try (symbol "neg"    ) >> Neg    <$> reg  <.> reg
    , try (symbol "add"    ) >> Add    <$> reg  <.> reg  <.> reg
    , try (symbol "addi"   ) >> Addi   <$> reg  <.> reg  <.> imm
    , try (symbol "sub"    ) >> Sub    <$> reg  <.> reg  <.> reg
    , try (symbol "subi"   ) >> Subi   <$> reg  <.> reg  <.> imm
    , try (symbol "mult"   ) >> Mult   <$> reg  <.> reg  <.> reg
    , try (symbol "multi"  ) >> Multi  <$> reg  <.> reg  <.> imm
    , try (symbol "div"    ) >> Div    <$> reg  <.> reg  <.> reg
    , try (symbol "divi"   ) >> Divi   <$> reg  <.> reg  <.> imm
    , try (symbol "mov.s"  ) >> Movs   <$> freg <.> freg
    , try (symbol "neg.s"  ) >> Negs   <$> freg <.> freg
    , try (symbol "add.s"  ) >> Adds   <$> freg <.> freg <.> freg
    , try (symbol "sub.s"  ) >> Subs   <$> freg <.> freg <.> freg
    , try (symbol "mul.s"  ) >> Muls   <$> freg <.> freg <.> freg
    , try (symbol "div.s"  ) >> Divs   <$> freg <.> freg <.> freg
    , try (symbol "srl"    ) >> Srl    <$> reg  <.> reg  <.> imm
    , try (symbol "sll"    ) >> Sll    <$> reg  <.> reg  <.> imm
    , try (symbol "li"     ) >> Li     <$> reg  <.> imm
    , try (symbol "la"     ) >> La     <$> reg  <.> labelI
    , try (symbol "lwl"    ) >> Lwl    <$> reg  <.> labelF
    , try (symbol "l.sl"   ) >> Lsl    <$> freg <.> labelF
    , try (symbol "beq"    ) >> Beq    <$> reg  <.> reg  <.> labelI
    , try (symbol "bne"    ) >> Bne    <$> reg  <.> reg  <.> labelI
    , try (symbol "blt"    ) >> Blt    <$> reg  <.> reg  <.> labelI
    , try (symbol "bgt"    ) >> Bgt    <$> reg  <.> reg  <.> labelI
    , try (symbol "c.eq.s" ) >> Ceqs   <$> freg <.> freg <.> labelI
    , try (symbol "c.le.s" ) >> Cles   <$> freg <.> freg <.> labelI
    , try (symbol "c.lt.s" ) >> Clts   <$> freg <.> freg <.> labelI
    , try (symbol "sin"    ) >> Sin    <$> freg <.> freg
    , try (symbol "cos"    ) >> Cos    <$> freg <.> freg
    , try (symbol "atan"   ) >> Atan   <$> freg <.> freg
    , try (symbol "floor"  ) >> Floor  <$> freg <.> freg
    , try (symbol "sqrt"   ) >> Sqrt   <$> freg <.> freg
    , try (symbol "ftoi"   ) >> Ftoi   <$> reg  <.> freg
    , try (symbol "itof"   ) >> Itof   <$> freg <.> reg
    , try (symbol "j"      ) >> J      <$> labelI
    , try (symbol "jr"     ) >> Jr     <$> reg
    , try (symbol "jal"    ) >> Jal    <$> labelI
    , try (symbol "jalr"   ) >> Jalr   <$> reg
    , try (symbol "print_i") >> PrintI <$> reg
    , try (symbol "print_f") >> PrintF <$> freg
    , try (symbol "print_c") >> PrintC <$> reg
    , try (symbol "read_i" ) >> ReadI  <$> reg
    , try (symbol "read_f" ) >> ReadF  <$> freg
    , try (symbol "exit"   ) >> return Exit
    , try (symbol "print_b") >> PrintB <$> reg
    -- base + offset
    -----------------
    , try (symbol "lwr" ) >> flip <$> (Lwr <$> reg ) <.> option (Imm 0) imm <*> parens reg
    , try (symbol "l.sr") >> flip <$> (Lsr <$> freg) <.> option (Imm 0) imm <*> parens reg
    , try (symbol "sw"  ) >> flip <$> (Sw  <$> reg ) <.> option (Imm 0) imm <*> parens reg
    , try (symbol "s.s" ) >> flip <$> (Ss  <$> freg) <.> option (Imm 0) imm <*> parens reg
    ]

-------------------------------------------------------------------------------
-- Lexer
-------------------------------------------------------------------------------

---------------
-- Data Type --
---------------

reg :: Parser Reg
reg = lexeme (fmap strToReg $ (++) <$> string "$" <*> identifier) <?> "reg"

freg :: Parser FReg
freg = lexeme (fmap strToFReg $ (++) <$> string "$f" <*> (show <$> natural)) <?> "freg"

labelF :: Parser LabelF
labelF = LabelF <$> identifier <?> "labelF"

labelI :: Parser LabelI
labelI =  LabelI <$> identifier <?> "labelI"

labelFDef :: Parser LabelF
labelFDef = labelF <* colon <?> "labelF def"

labelIDef :: Parser LabelI
labelIDef = labelI <* colon <?> "labelI def"

imm :: Parser Imm
imm = Imm <$> int16

----------------
-- pure lexer --
----------------

def :: P.GenLanguageDef String u Identity
def = haskellDef
  { P.identStart  = P.identStart haskellDef <|> char '_'
  , P.identLetter = alphaNum <|> oneOf "_'."
  , P.commentLine = "#"
  }

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser def

lexeme :: Parser a -> Parser a
lexeme = P.lexeme lexer

identifier :: Parser String
identifier = P.identifier lexer

hex :: Parser Word32 -- x始まりらしい
hex = lexeme (fromIntegral <$> (char '0' >> P.hexadecimal lexer)) <?> "hex"

natural :: Parser Int
natural = fromIntegral <$> P.natural lexer

int16 :: Parser Int16
int16 = fromIntegral <$> P.integer lexer

symbol :: String -> Parser String
symbol s = lexeme (string s <* notFollowedBy (P.identLetter def))
        <?> ("symbol `"++s++"`")

comma :: Parser String
comma  = P.comma lexer

colon :: Parser String
colon = P.colon lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

(<.>) :: Parser (a -> b) -> Parser a -> Parser b
f <.> x = f <*> (comma *> x)
infixl 4 <.>

