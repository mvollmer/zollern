all: sieve

z0: z0.c
	cc -std=c99 -g -o z0 z0.c

sieve-c: sieve.c
	gcc -std=c99 -o sieve-c sieve.c

sieve: sieve.z0 z0
	./z0 <sieve.z0 >sieve && chmod a+x sieve

t.o: t.c Makefile
	cc -O6 -g -o t.o -c t.c
	objdump -d t.o

exit.o:	exit.s
	as -o exit.o exit.s
	objdump -d exit.o
