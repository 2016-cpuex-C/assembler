{-# LANGUAGE TemplateHaskell #-}

module Parser where

import           Types

import           Prelude hiding (pred,Ordering(..))
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

data S = S { _floatMap :: Map LabelF Word32
           , _instMap  :: Map LabelI Word32
           , _floatCnt :: Word32
           , _instCnt  :: Word32
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
  optional $ void $ symbol ".globl"
  {-optional $ void $ symbol "main"-}
  is <- concat <$> many block
  s  <- getState
  whiteSpace
  eof
  return $ ParseResult xs is (s^.floatMap) (s^.instMap)

datum :: Parser Word32
datum = try labelFDef >>= addLabelF >> symbol ".word" >> incFloatCnt >> hex
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
      symbol' "move"      >> Move     <$> reg  <.> reg
    , symbol' "neg"       >> Neg      <$> reg  <.> reg
    , symbol' "add"       >> Add      <$> reg  <.> reg  <.> reg
    , symbol' "addi"      >> Addi     <$> reg  <.> reg  <.> imm
    , symbol' "sub"       >> Sub      <$> reg  <.> reg  <.> reg
    , symbol' "mult"      >> Mult     <$> reg  <.> reg  <.> reg
    , symbol' "multi"     >> Multi    <$> reg  <.> reg  <.> imm
    , symbol' "div"       >> Div      <$> reg  <.> reg  <.> reg
    , symbol' "divi"      >> Divi     <$> reg  <.> reg  <.> imm
    , symbol' "mov.s"     >> Movs     <$> freg <.> freg
    , symbol' "neg.s"     >> Negs     <$> freg <.> freg
    , symbol' "add.s"     >> Adds     <$> freg <.> freg <.> freg
    , symbol' "sub.s"     >> Subs     <$> freg <.> freg <.> freg
    , symbol' "mul.s"     >> Muls     <$> freg <.> freg <.> freg
    , symbol' "div.s"     >> Divs     <$> freg <.> freg <.> freg
    , symbol' "srl"       >> Srl      <$> reg  <.> reg  <.> reg
    , symbol' "sll"       >> Sll      <$> reg  <.> reg  <.> reg
    , symbol' "srli"      >> Srli     <$> reg  <.> reg  <.> imm
    , symbol' "slli"      >> Slli     <$> reg  <.> reg  <.> imm
    , symbol' "li"        >> Li       <$> reg  <.> imm
    , symbol' "la"        >> La       <$> reg  <.> labelI
    , symbol' "l.sl"      >> Lsl      <$> freg <.> labelF
    , symbol' "beq"       >> Beq      <$> reg  <.> reg  <.> labelI
    , symbol' "bne"       >> Bne      <$> reg  <.> reg  <.> labelI
    , symbol' "blt"       >> Blt      <$> reg  <.> reg  <.> labelI
    , symbol' "bgt"       >> Bgt      <$> reg  <.> reg  <.> labelI
    , symbol' "beqi"      >> Beqi     <$> reg  <.> imm5 <.> labelI
    , symbol' "bnei"      >> Bnei     <$> reg  <.> imm5 <.> labelI
    , symbol' "blti"      >> Blti     <$> reg  <.> imm5 <.> labelI
    , symbol' "bgti"      >> Bgti     <$> reg  <.> imm5 <.> labelI
    , symbol' "c.eq.s"    >> Ceqs     <$> freg <.> freg <.> labelI
    , symbol' "c.le.s"    >> Cles     <$> freg <.> freg <.> labelI
    , symbol' "c.lt.s"    >> Clts     <$> freg <.> freg <.> labelI
    , symbol' "j"         >> J        <$> labelI
    , symbol' "jr"        >> Jr       <$> reg
    , symbol' "jal"       >> Jal      <$> labelI
    , symbol' "jalr"      >> Jalr     <$> reg
    , symbol' "print_c"   >> PrintC   <$> reg
    , symbol' "read_i"    >> ReadI    <$> reg
    , symbol' "read_f"    >> ReadF    <$> freg
    , symbol' "and"       >> And      <$> reg  <.> reg  <.> reg
    , symbol' "or"        >> Or       <$> reg  <.> reg  <.> reg
    , symbol' "andi"      >> Andi     <$> reg  <.> reg  <.> imm
    , symbol' "ori"       >> Ori      <$> reg  <.> reg  <.> imm
    , symbol' "swap"      >> Swap     <$> reg  <.> reg
    , symbol' "swap.s"    >> Swaps    <$> freg <.> freg
    , symbol' "select"    >> Select   <$> reg  <.> reg  <.> reg  <.> reg
    , symbol' "select.s"  >> Selects  <$> freg <.> freg <.> freg <.> freg
    , symbol' "cmp"       >> Cmp      <$> pred <.> reg  <.> reg  <.> reg
    , symbol' "cmpi"      >> Cmpi     <$> pred <.> reg  <.> reg  <.> imm5
    , symbol' "cmp.s"     >> Cmps     <$> pred <.> reg  <.> freg <.> freg
    , symbol' "cvt.s.w"   >> Cvtsw    <$> freg <.> reg
    , symbol' "cvt.w.s"   >> Cvtws    <$> reg  <.> freg
    , symbol' "madd.s"    >> MAdds    <$> freg <.> freg <.> freg <.> freg
    , symbol' "exit"      >> return Exit

    -- base + offset
    -----------------
    , symbol' "lwr"     >> flip <$> (Lwr <$> reg ) <.> imm' <*> parens reg
    , symbol' "l.sr"    >> flip <$> (Lsr <$> freg) <.> imm' <*> parens reg
    , symbol' "sw"      >> flip <$> (Sw  <$> reg ) <.> imm' <*> parens reg
    , symbol' "s.s"     >> flip <$> (Ss  <$> freg) <.> imm' <*> parens reg
    ]
    where symbol' = try.symbol
          imm'    = option (Imm 0) imm

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

imm5 :: Parser Imm5
imm5 = Imm5 <$> int16

pred :: Parser Predicate
pred = choice [
    symbol' "eq" >> return EQ
  , symbol' "ne" >> return NE
  , symbol' "le" >> return LE
  , symbol' "ge" >> return GE
  , symbol' "lt" >> return LT
  , symbol' "gt" >> return GT
  ]
  where symbol' = try.symbol

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

