# WNG Compiler

## Description

WNG is a toy language I am creating to develop my skills as a compiler developer. The acronym "WNG" stands for "WNG's Not Go".
WNG is a statically-typed, compiled language powered by LLVM.

## Grammar

A formal specification of the language's grammar can be found in the [grammar.lalrpop](src/grammar.lalrpop) file.
The grammar is LR(1) parseable and the parser is generated using the [LALRPOP](https://github.com/lalrpop/lalrpop) parser generator for Rust.

## Usage

Below is a short example program written in WNG that calculates and prints the first 10 fibonacci numbers.

```wng
fn fib(n: Number) -> Number {
  if n == 0 {
    return 0;
  }

  if n == 1 {
    return 1;
  }

  fib(n - 1) + fib(n - 2)
}

fn main() {
  for (i = 0; i < 10; i = i + 1;) {
    print fib(i);
  }
}
```
