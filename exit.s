.globl _start
_start:
        pushq $0x7FFFFFFF
        popq %rbx
        syscall
