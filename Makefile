all: sieve.test

z0: z0.c
	cc -std=c99 -g -o z0 z0.c

%: %.z0 z0
	./z0 $*.z0 $*

%.test: %
	./$* >$*.out
	cmp $*.out $*.expected

t.o: t.c Makefile
	cc -O6 -g -o t.o -c t.c
	objdump -d t.o

exit.o:	exit.s
	as -o exit.o exit.s
	objdump -d exit.o

sieve-c: sieve.c
	gcc -O6 -std=c99 -o sieve-c sieve.c
