all: z0 zfb hello sieve dots

# Assembler

z0: z0.c
	cc -std=c99 -g -o z0 z0.c

# Programs

LIBS = std.z0 fb.z0 testlib.z0

hello_ADD = std.z0
sieve_ADD = std.z0
dots_ADD  = std.z0 fb.z0
test_ADD = testlib.z0

# Testing

check: test
	./test

# Assembling

%: %.z0 $(LIBS) pre.z0 z0
	./z0 pre.z0 $($*_ADD) $*.z0 $*
#       objdump -d $*

# Framebuffer

zfb: zfb.c
	gcc -g -o zfb zfb.c $$(pkg-config --libs --cflags sdl2) -lrt -lX11

# Stuff

tt.dump: tt.c Makefile
	gcc -O6 -std=c99 -c tt.c
	objdump -d tt.o

asm.dump: asm.s Makefile
	gcc -O6 -std=c99 -c asm.s
	objdump -d asm.o

sieve-c: sieve.c
	gcc -O6 -std=c99 -o sieve-c sieve.c
