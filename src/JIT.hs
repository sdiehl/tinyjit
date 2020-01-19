{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module JIT
  ( allocateMemory,
    codePtr,
    vecPtr,
    asciz,
    extern,
    heapPtr,
    getFunction,
    jit,
  )
where

import Control.Exception
import Control.Monad (when)
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Data.ByteString.Lazy.Char8
import Data.Int
import Data.Monoid
import Data.Typeable
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr
import Foreign.Storable
import System.Posix.DynamicLinker
import System.Posix.Types
import Unsafe.Coerce

-------------------------------------------------------------------------------
-- Prot Option
-------------------------------------------------------------------------------

newtype ProtOption = ProtOption CInt
  deriving (Eq, Show, Ord, Num, Bits)

newtype MmapOption = MmapOption CInt
  deriving (Eq, Show, Ord, Num, Bits)

protExec :: ProtOption
protExec = ProtOption 0x01

protRead :: ProtOption
protRead = ProtOption 0x04

protWrite :: ProtOption
protWrite = ProtOption 0x02

protNone :: ProtOption
protNone = ProtOption 0x0

mmapNone :: MmapOption
mmapNone = MmapOption 0x0

mmapAnon :: MmapOption
mmapAnon = MmapOption 0x20

mmapPrivate :: MmapOption
mmapPrivate = MmapOption 0x02

instance Semigroup ProtOption where
  (ProtOption a) <> (ProtOption b) = ProtOption (a .|. b)

instance Monoid ProtOption where
  mempty = protNone

instance Semigroup MmapOption where
  (MmapOption a) <> (MmapOption b) = MmapOption (a .|. b)

instance Monoid MmapOption where
  mempty = mmapNone

-------------------------------------------------------------------------------
-- JIT Memory
-------------------------------------------------------------------------------

mapAnon, mapPrivate :: MmapFlags
mapAnon = 0x20
mapPrivate = 0x02

newtype MmapFlags = MmapFlags {unMmapFlags :: CInt}
  deriving (Eq, Show, Ord, Num, Bits)

foreign import ccall "dynamic"
  mkFun :: FunPtr (IO Int) -> IO Int

getFunction :: Ptr Word8 -> IO Int
getFunction mem = do
  let fptr = unsafeCoerce mem :: FunPtr (IO Int)
  mkFun fptr

jit :: Ptr Word8 -> [Word8] -> IO (IO Int)
jit mem machCode = do
  code <- codePtr machCode
  withForeignPtr (vecPtr code) $ \ptr -> do
    {-print ptr -- Pointer to Haskell array-}
    {-print mem -- Pointer to JIT memory-}
    copyBytes mem ptr (8 * 6)
  return (getFunction mem)

data MmapException = MmapException
  deriving (Eq, Ord, Show, Typeable)

instance Exception MmapException

foreign import ccall unsafe "sys/mman.h mmap"
  c_mmap :: Ptr () -> CSize -> ProtOption -> MmapFlags -> Fd -> COff -> IO (Ptr a)

mmap ::
  Ptr () ->
  CSize ->
  ProtOption ->
  MmapFlags ->
  Fd ->
  COff ->
  IO (Ptr Word8)
mmap addr len prot flags fd offset = do
  ptr <- c_mmap addr len prot flags fd offset
  when (ptr == intPtrToPtr (-1)) $ throwIO MmapException
  return ptr

codePtr :: [Word8] -> IO (VM.IOVector Word8)
codePtr = V.thaw . V.fromList

vecPtr :: Storable a => VM.MVector s a -> ForeignPtr a
vecPtr = fst . VM.unsafeToForeignPtr0

allocateMemory :: CSize -> IO (Ptr Word8)
allocateMemory size = mmap nullPtr size pflags mflags (-1) 0
  where
    pflags = protRead <> protWrite
    mflags = mapAnon .|. mapPrivate

asciz :: [Char] -> IO Word32
asciz str = do
  ptr <- newCString (str ++ ['\n'])
  return $ heapPtr ptr

extern :: String -> IO Word32
extern name = do
  dl <- dlopen "" [RTLD_LAZY, RTLD_GLOBAL]
  fn <- dlsym dl name
  return $ heapPtr $ castFunPtrToPtr fn

heapPtr :: Ptr a -> Word32
heapPtr = fromIntegral . ptrToIntPtr
