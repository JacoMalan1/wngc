section .text

extern main
global _start

_start:
  call main
  
  xor edi, edi
  mov rax, 60
  syscall

section .note.GNU-stack progbits
