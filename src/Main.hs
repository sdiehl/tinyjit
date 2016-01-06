module Main (
  main
) where

import Data.Int
import Data.Word
import Data.Monoid

import Foreign.C.Types

import JIT
import Assembler

-- Example 1
arith :: X86 ()
arith = do
  mov rax (I 18)
  add rax (I 4)
  sub rax (I 2)
  inc rax
  dec rax
  inc rax
  imul rax (I 2)
  mul rax
  ret

-- Example 2
factorial :: Int64 -> X86 ()
factorial n = do
  mov rcx (I n)
  mov rax (I 1)
  l1 <- label
  mul rcx
  loop l1
  ret

-- Example 3
printf :: Word32 -> Word32 -> X86 ()
printf fnptr msg = do
  push rbp
  mov rbp rsp
  mov rdi (A msg)
  call (A fnptr)
  pop rbp
  mov rax (I 0)
  ret

dump :: [Word8] -> IO ()
dump = mapM_ (Prelude.putStrLn . hex)

main :: IO ()
main = do
  let jitsize = 256*1024

  fn  <- extern "printf"
  msg <- asciz "Hello Haskell"
  mem <- allocateMemory jitsize

  {-let jitm = assemble mem arith-}
  let jitm = assemble mem (factorial 5)
  {-let jitm = assemble mem (printf fn msg)-}

  case jitm of
    Left err -> putStrLn err
    Right jitst -> do
      let machCode = _mach jitst
      dump machCode

      fn <- jit mem machCode
      res <- fn
      putStrLn $ "Result: " <> show res
