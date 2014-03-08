module Jit (run, compile) where


import qualified Parser
import Data.Binary
import Data.Bits
import qualified Data.ByteString as BS


data Instr =
    Push Integer
  | Add
  | Mul
  deriving (Eq, Show)


asStackCode :: Parser.Expr -> [Instr]
asStackCode expr = compile [] expr
  where
    compile ins (Parser.Num n) = (Push n) : ins
    compile ins (Parser.Add e1 e2) = Add : (compile (compile ins e1) e2)
    compile ins (Parser.Mul e1 e2) = Mul : (compile (compile ins e1) e2)


run :: Parser.Expr -> Integer
run = undefined


littleEndian :: Word64 -> [Word8]
littleEndian n = map getByte [0 .. 7]
  where 
    getByte i = fromIntegral $ 0xFF .&. (n `shiftR` (8 * i))


compile :: Parser.Expr -> BS.ByteString
compile = BS.pack . concat . reverse . map compile' . asStackCode
  where
    -- movabs $0x..., %rax
    -- push %rax
    compile' (Push n) =
      concat [[0x48, 0xB8], littleEndian $ fromInteger n, [0x50]]
    -- pop %rax
    -- add %rax, (%rsp)
    compile' Add = [0x58, 0x48, 0x01, 0x04, 0x24]
    -- pop %rax
    -- imul (%rsp), %rax
    -- mov %rax, (%rsp)
    compile' Mul = [0x58, 0x48, 0x0F, 0x04, 0x24, 0x48, 0x89, 0x04, 0x24]

