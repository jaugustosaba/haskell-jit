import Lexer
import Parser
import Interpreter
import Jit
import Text.Printf

main :: IO ()
main = do
  input <- getContents
  let tokens = tokenize input
  let expr = parse tokens
  let res = eval expr
  printf "result: %d\n" res
  printf "jit result: %d\n" $ runJit expr
