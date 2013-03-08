all: z1

z0: z0.c
	cc -std=c99 -g -o z0 z0.c

z1: z0 z1.z
	./z0 <z1.z >z1 && chmod a+x z1
	objdump -d z1

t.o: t.c
	cc -o t.o -c t.c
	objdump -d t.o

exit.o:	exit.s
	as -o exit.o exit.s
	objdump -d exit.o
