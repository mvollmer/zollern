all: z0 zfb hello sieve dots hellofb \
     z1

V=

# Assembler

z0: z0.c
	cc -std=c99 -g -o z0 z0.c

# Programs

LIBS = std.z0 fb.z0 testlib.z0 font.z0

hello_ADD = std.z0
sieve_ADD = std.z0
dots_ADD  = std.z0 fb.z0
test_ADD = testlib.z0
hellofb_ADD = std.z0 fb.z0 font.z0

z1_ADD = std.z0

# Testing

check: test
	./test

# Assembling

%: %.z0 $(LIBS) pre.z0 z0
	./z0 pre.z0 $($*_ADD) $*.z0 $*
	@if [ -n "$V" ]; then objdump -d $*; fi

# Framebuffer

zfb: zfb.c
	gcc -g -o zfb zfb.c $$(pkg-config --libs --cflags sdl2) -lrt -lX11

zttf: zttf.c
	gcc -g -o zttf zttf.c $$(freetype-config --cflags --libs)

font.z0: zttf Makefile
	./zttf /usr/share/fonts/dejavu/DejaVuSansMono.ttf 14 mono-14 >font.z0

# Stuff

tt.dump: tt.c Makefile
	gcc -O6 -std=c99 -c tt.c
	objdump -d tt.o

asm.dump: asm.s Makefile
	gcc -O6 -std=c99 -c asm.s
	objdump -d asm.o

sieve-c: sieve.c
	gcc -O6 -std=c99 -o sieve-c sieve.c
