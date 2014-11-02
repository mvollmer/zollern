all: sieve.test

z0: z0.c
	cc -std=c99 -g -o z0 z0.c

%: pre.z0 %.z0 z0
	./z0 pre.z0 $*.z0 $*
	objdump -d $*

%.test: %
	./$* >$*.out
	cmp $*.out $*.expected

sieve.dump: sieve.c Makefile
	gcc -O6 -std=c99 -o sieve-c sieve.c
	objdump -d sieve-c >sieve.dump

sieve.s: sieve.c Makefile
	gcc -O6 -std=c99 -S sieve.c

tt.dump: tt.c Makefile
	gcc -O6 -std=c99 -c tt.c
	objdump -d tt.o

asm.dump: asm.s Makefile
	gcc -O6 -std=c99 -c asm.s
	objdump -d asm.o

check: z0
	./z0 pre.z0 testlib.z0 test.z0 test
	objdump -d ./test
	./test
