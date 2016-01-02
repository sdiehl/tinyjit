Haskell JIT Example
-------------------

Tiny example of building a intermediate language that JIT compiles Haskell DSL
into x86-64 machine code.

Usage
-----

The factorial function can be written in assembly, taking the input value in
``%rcx`` and computing the resulting value in ``%rax``.

```perl
.global main

main:
        mov rcx, 5
        mov rax, 1
.factor:
        mul rcx
        loop .factor
        ret
```

In our Haskell logic we compose these operations inside of the ``X86`` monad.

```haskell
factorial :: Int64 -> X86 ()
factorial n = do
  mov rcx (I n)
  mov rax (I 1)
  l1 <- label
  mul rcx
  loop l1
  ret
```

The resulting logic can be JIT compiled inside of Haskell and invoked from
inside the Haskell runtime by calling out to the JIT'd memory.

```haskell
main :: IO ()
main = do
  let jitsize = 256*1024
  mem <- allocateMemory jitsize
  case jitm of
    Left err -> putStrLn err
    Right jitst -> do
      let machCode = _mach jitst
      fn <- jit mem machCode
      res <- fn
      putStrLn $ "Result: " <> show res
```

License
-------

Released under the MIT License.
Copyright (c) 2016, Stephen Diehl
