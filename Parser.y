{
module Parser where

import qualified Lexer
}

%name parse
%tokentype { Lexer.Token }
%error { parserError }

%token
  num    { Lexer.Num $$ }
  add    { Lexer.Add }
  mul    { Lexer.Mul }
  lparen { Lexer.LParen }
  rparen { Lexer.RParen }

%left add
%left mul

%%

expr:
    num           { Num $1 }
  | expr add expr { Add $1 $3 }
  | expr mul expr { Mul $1 $3 }
  | lparen expr rparen { $2 }

{

data Expr =
    Num Integer
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Eq, Show)

parserError :: [Lexer.Token] -> a
parserError _ = error "Syntax error"

}
