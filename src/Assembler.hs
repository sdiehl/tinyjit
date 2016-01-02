module Assembler (
  Reg,
  Instr,
  X86,
  Val(..),
  JITMem(..),

  assemble,

  ret,
  add,
  sub,
  push,
  pop,
  call,
  mul,
  imul,
  mov,
  nop,
  inc,
  dec,
  loop,

  rax,
  rbp,
  rsp,
  rdi,
  rdx,
  rsi,
  rcx,

  label,

  prologue,
  epilogue,

  hex,
  bytes,
) where

import Data.Int
import Data.Word
import Data.Bits
import Data.Monoid
import Data.Typeable
import Data.Binary.Put
import Data.ByteString.Lazy.Char8 (ByteString, unpack)
import qualified Data.ByteString.Internal as BS (c2w, w2c)

import Numeric (showHex, showIntAtBase)

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import JIT

-------------------------------------------------------------------------------
-- X86 Monad
-------------------------------------------------------------------------------

data Reg
  = RAX  -- Accumulator
  | RCX  -- Counter (Loop counters)
  | RDX  -- Data
  | RBX  -- Base / General Purpose
  | RSP  -- Current stack pointer
  | RBP  -- Previous Stack Frame Link
  | RSI  -- Source Index Pointer
  | RDI  -- Destination Index Pointer
  deriving (Eq, Show)

data Val
  = I Int64      -- int
  | R Reg        -- register
  | A Word32     -- Addr
  | L Word32     -- Label
  deriving (Eq, Show)

data Instr
  = Ret
  | Mov Val Val
  | Add Val Val
  | Sub Val Val
  | Mul Val
  | IMul Val Val
  | Xor Val Val
  | Inc Val
  | Dec Val
  | Push Val
  | Pop Val
  | PreCall Val
  | Call Val
  | Loop Val
  | Nop
  | Syscall
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

data JITMem = JITMem
 { _instrs :: [Instr]
 , _mach   :: [CUChar]
 , _icount :: Word32
 , _memptr :: Word32
 , _memoff :: Word32
 } deriving (Eq, Show)

type X86 a = StateT JITMem (Except String) a

istate :: Word32 -> JITMem
istate start = JITMem
  { _instrs = []
  , _icount = 0
  , _mach   = []
  , _memptr = start
  , _memoff = start
  }

emit :: [CUChar] -> X86 ()
emit i = modify $ \s -> s
  { _mach = _mach s ++ i
  , _memoff = _memoff s + fromIntegral (length i)
  }

imm :: Integral a => a -> X86 ()
imm = emit . bytes

-- Instructions

ret :: X86 ()
ret = do
  emit [0xc3]

add :: Val -> Val -> X86 ()
add (R l) (I r) = do
  emit [0x48]              -- REX prefix
  emit [0x83]              -- ADD
  emit [0xc0 .|. index l]
  emit [fromIntegral r]
add (R l) (R r) = do
  emit [0x48]              -- REX prefix
  emit [0x01]              -- ADD
  emit [0xc0 .|. index r `shiftL` 3 .|. index l]
add _ _ = nodef

sub :: Val -> Val -> X86 ()
sub (R l) (I r) = do
  emit [0x48]              -- REX prefix
  emit [0x83]              -- SUB
  emit [0xc0 .|. 0x05 `shiftL` 3 .|. index l]
  emit [fromIntegral r]
sub (R l) (R r) = do
  emit [0x48]              -- REX prefix
  emit [0x29]              -- SUB
  emit [0xc0 .|. index r `shiftL` 3 .|. index l]

push :: Val -> X86 ()
push (R l) = do
  emit [0x50 + index l]
push _ = nodef

pop :: Val -> X86 ()
pop (R l) = do
  emit [0x58 + index l]
pop _ = nodef

call :: Val -> X86 ()
call (A dst) = do
  emit [0xE8]
  src <- gets _memoff
  imm (dst - (src + 5))
call _ = nodef

mul :: Val -> X86 ()
mul (R l) = do
  emit [0x48]
  emit [0xF7]
  emit [0xE0 .|. index l]
mul _ = nodef

imul :: Val -> Val -> X86 ()
imul (R l) (I r) = do
  emit [0x48]
  emit [0x6B]
  emit [0xc0 .|. index l]
  emit [fromIntegral r]
imul (R l) (R r) = do
  emit [0x48]
  emit [0x0F]
  emit [0xAF]
  emit [0xC0 .|. index r `shiftL` 3 .|. index l]
imul _ _ = nodef

mov :: Val -> Val -> X86 ()
mov (R dst) (I src) = do
  emit [0x48] -- REX.W prefix
  emit [0xC7]
  emit [0xC0 .|. (index dst .&. 7)]
  imm src
mov (R dst) (A src) = do
  emit [0x48] -- REX.W prefix
  emit [0xC7]
  emit [0xC7]
  imm src
mov (R dst) (R src) = do
  emit [0x48]              -- REX.W prefix
  emit [0x89]              -- MOV
  emit [0xC0 .|. index src `shiftL` 3 .|. index dst]
mov _ _ = nodef

nop :: X86 ()
nop = do
  emit [0x90]

inc :: Val -> X86()
inc (R dst) = do
  emit [0x48]              -- REX prefix
  emit [0xFF]              -- INC
  emit [0xc0 + index dst]
inc _ = nodef

dec :: Val -> X86()
dec (R dst) = do
  emit [0x48]              -- REX prefix
  emit [0xFF]              -- DEC
  emit [0xc0 + (index dst + 8)]
dec _ = nodef

loop :: Val -> X86()
loop (A dst) = do
  emit [0xE2]
  src <- gets _memoff
  ptr <- gets _memptr
  emit [fromIntegral $ dst - src]

syscall :: X86 ()
syscall = do
  emit [0x0f]
  emit [0x05]

-- Functions

prologue :: X86 ()
prologue = do
  push rbp
  mov rbp rsp

epilogue :: X86 ()
epilogue = do
  pop rax
  mov rsp rbp
  pop rbp
  ret

-- Registers

rax :: Val
rax = R RAX

rbp :: Val
rbp = R RBP

rsp :: Val
rsp = R RSP

rdi :: Val
rdi = R RDI

rsi :: Val
rsi = R RSI

rdx :: Val
rdx = R RDX

rcx :: Val
rcx = R RCX

label :: X86 Val
label = do
  addr <- gets _memoff
  ptr  <- gets _memptr
  return (A addr)

nodef :: X86 ()
nodef = lift $ throwE "Invalid operation"

index :: Reg -> CUChar
index x = case x of
  RAX -> 0
  RCX -> 1
  RDX -> 2
  RBX -> 3
  RSP -> 4
  RBP -> 5
  RSI -> 6
  RDI -> 7

assemble :: Ptr a -> X86 b -> Either String JITMem
assemble start = runExcept . flip execStateT (istate ptr)
  where
    ptr = heapPtr start

hex :: (Integral a, Show a) => a -> String
hex x = showHex x ""

bytes :: Integral a => a -> [CUChar]
bytes x = fmap (CUChar. BS.c2w) bs
  where
    bs = unpack $ runPut $ putWord32le (fromIntegral x)
