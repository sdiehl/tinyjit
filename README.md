Haskell JIT Example
-------------------

[![Build Status](https://travis-ci.org/sdiehl/tinyjit.svg)](https://travis-ci.org/sdiehl/tinyjit)
[![MIT License](http://img.shields.io/badge/license-mit-blue.svg)](https://github.com/sdiehl/tinyjit/blob/master/LICENSE)

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
  let jitm = assemble mem (factorial 5)

  case jitm of
    Left err -> putStrLn err
    Right jitst -> do
      let machCode = _mach jitst
      fn <- jit mem machCode
      res <- fn
      putStrLn $ "Result: " <> show res
```

The machine code is generated.

```
48 c7 c1 05 00 00 00 48 c7 c0 01 00 00 00 48 f7 e1 e2 fc c3
```

And executed to yield the result:

```
Result: 120
```

License
-------

Released under the MIT License.
Copyright (c) 2016-2020, Stephen Diehl
