{
module Lexer (
  Token(..),
  tokenize
  ) where
}

%wrapper "basic"

$digit = 0-9

tokens :-
  $white+;
  \+        { \s -> Add }
  \*        { \s -> Mul }
  $digit+   { \s -> Num (read s) }
  \(        { \s -> LParen }
  \)        { \s -> RParen }
{

data Token =
    Add
  | Mul
  | Num Integer
  | LParen
  | RParen
  deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize = alexScanTokens

}
