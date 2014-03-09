haskell-jit
=========

Simple Just-in-time (JIT) for arithmetic expressions done in Haskell.

## System Requirements
* x86_64 
* Ubuntu >= 12.12 / OS X >= 10.6
* gcc >= 4.8
* gdb >= 7.61
* ghc >= 7.6.3

## Files

Main files description.

### Lexer.x

Lexer (alex)

### Parser.y

Parser (happy)

### Interpreter.hs

AST Interpreter.

### Jit.hs

Just-in-time (JIT) code + FFI code.

### Main.hs

Compiler's main file.

### mmap-x86_64.c

Takes executable memory from OS to execute code generated from haskell code.

## Compiling

    $ cabal configure
    $ cabal build
    
to clean compiled code:

    $ cabal clean

## Sample

    1+(2+3)*4

## Native code (dump from GDB)

    0x00007ffff7ff7000:	push   %rbp
    0x00007ffff7ff7001:	mov    %rsp,%rbp
    0x00007ffff7ff7004:	movabs $0x1,%rax
    0x00007ffff7ff700e:	push   %rax
    0x00007ffff7ff700f:	movabs $0x2,%rax
    0x00007ffff7ff7019:	push   %rax
    0x00007ffff7ff701a:	movabs $0x3,%rax
    0x00007ffff7ff7024:	push   %rax
    0x00007ffff7ff7025:	pop    %rax
    0x00007ffff7ff7026:	add    %rax,(%rsp)
    0x00007ffff7ff702a:	movabs $0x4,%rax
    0x00007ffff7ff7034:	push   %rax
    0x00007ffff7ff7035:	pop    %rax
    0x00007ffff7ff7036:	imul   (%rsp),%rax
    0x00007ffff7ff703b:	mov    %rax,(%rsp)
    0x00007ffff7ff703f:	pop    %rax
    0x00007ffff7ff7040:	add    %rax,(%rsp)
    0x00007ffff7ff7044:	pop    %rax
    0x00007ffff7ff7045:	pop    %rbp
    0x00007ffff7ff7046:	retq   $0x0
