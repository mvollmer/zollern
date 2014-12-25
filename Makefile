all: hello

# Assembler

z0: z0.c
	cc -std=c99 -g -o z0 z0.c

# Assembling

%: %.z0 $($*_ADD) pre.z0 z0
	./z0 pre.z0 $($*_ADD) $*.z0 $*
	objdump -d $*

# Programs

hello_ADD = std.z0

# Testing

test_ADD = testlib.z0

check: test
	./test

# Stuff

tt.dump: tt.c Makefile
	gcc -O6 -std=c99 -c tt.c
	objdump -d tt.o

asm.dump: asm.s Makefile
	gcc -O6 -std=c99 -c asm.s
	objdump -d asm.o

fb: fb.c
	gcc -o fb fb.c $$(pkg-config --libs --cflags sdl2) -lrt
