{-# LANGUAGE ForeignFunctionInterface #-}
module Jit (runJit) where

import qualified Parser
import GHC.Word
import Data.Bits
import qualified Data.ByteString as BS
import System.IO.Unsafe
import Foreign.C

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


foreign import ccall "runJit" c_runJit :: CString -> CSize -> IO CInt

runJit :: Parser.Expr -> Integer
runJit expr = toInteger $ unsafePerformIO $ BS.useAsCString code runJit
  where
    code = compile expr
    csize = fromIntegral $ BS.length code
    runJit cstr = c_runJit cstr csize


littleEndian :: Word64 -> [Word8]
littleEndian n = map getByte [0 .. 7]
  where 
    getByte i = fromIntegral $ 0xFF .&. (n `shiftR` (8 * i))


compile :: Parser.Expr -> BS.ByteString
compile expr = BS.pack $ concat [ prologue, code, epilogue ]
  where
    code = concat $ reverse $ map compile' $ asStackCode expr
    -- push %rbp
    -- mov %rsp, %rbp
    prologue = [0x55, 0x48, 0x89, 0xE5]
    -- pop %rbp
    -- retq $0x0
    epilogue = [0x58, 0x5D, 0xC2, 0x00, 0x00]
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
    compile' Mul = [0x58, 0x48, 0x0F, 0xAF, 0x04, 0x24, 0x48, 0x89, 0x04, 0x24]

