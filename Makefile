all: sieve.test

z0: z0.c
	cc -std=c99 -g -o z0 z0.c

%: %.z0 z0
	./z0 $*.z0 $*
	objdump -d $*

%.test: %
	./$* >$*.out
	cmp $*.out $*.expected

sieve-c: sieve.c
	gcc -O6 -std=c99 -o sieve-c sieve.c
