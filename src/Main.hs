module Main (
  main
) where

import Criterion
import Criterion.Main

import Data.Int
import Data.Word
import Data.Monoid

import Foreign.C.Types

import System.IO.Unsafe

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
factorial :: X86 ()
factorial = do
  mov rcx rdi
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

doit :: (Int -> IO Int) -> Int -> IO ()
doit f n = putStrLn $ "f " ++ show n ++ " = " ++ show (unsafePerformIO $ f n)


haskell_factorial :: Int -> Int
haskell_factorial 0 = 1
haskell_factorial n = n * haskell_factorial (n - 1)


main :: IO ()
main = do
  let jitsize = 256*1024

  fn  <- extern "printf"
  msg <- asciz "Hello Haskell"
  mem <- allocateMemory jitsize

  {-let jitm = assemble mem arith-}
  let jitm = assemble mem factorial
  {-let jitm = assemble mem (printf fn msg)-}

  case jitm of
    Left err -> putStrLn err
    Right jitst -> do
      let machCode = _mach jitst
      dump machCode

      fn <- jit mem machCode
      mapM_ (doit fn) [1..10]

      defaultMain [
        bench "hfact 200"   (nf haskell_factorial 200),
        bench "jitfact 200" (nf (unsafePerformIO . fn) 200),

        bench "hfact 20"   (nf haskell_factorial 20),
        bench "jitfact 20" (nf (unsafePerformIO . fn) 20),

        bench "hfact 3"    (nf haskell_factorial 3),
        bench "jitfact 3"  (nf (unsafePerformIO . fn) 3),

        bench "hfact 1"    (nf haskell_factorial 1),
        bench "jitfact 1"  (nf (unsafePerformIO . fn) 1)
                  ]
