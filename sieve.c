#define N 1000

char strbuf[512];
char intbuf[N];

long
my_strlen (char *str)
{
  long i = 0;
  for (i = 0; str[i]; i++)
    ;
  return i;
}

void
strint (char *buf, long i)
{
  char *p = buf;
  char t;
  do
    {
      *p++ = (i % 10) + '0';
      i /= 10;
    }
  while (i);
  *p = '\0';
  do
    {
      p -= 1;
      t = *p;
      *p = *buf;
      *buf = t;
      buf += 1;
    }
  while (p > buf);
}

void
init_intbuf ()
{
  for (long i = 0; i < N; i++)
    intbuf[i] = 0;
}

void
print_intbuf ()
{
  for (long i = 2; i < N; i += 1)
    if (!intbuf[i])
      {
        strint (strbuf, i);
        write (1, strbuf, my_strlen (strbuf));
        write (1, "\n", 1);
      }
}

void
set_intbuf (int f)
{
  for (long i = f+f; i < N; i += f)
    intbuf[i] = 1;
}

void
sieve_intbuf ()
{
  for (long p = 2; p*p < N; p++)
    if (intbuf[p] == 0)
      set_intbuf (p);
}

void
main ()
{
  init_intbuf ();
  sieve_intbuf ();
  print_intbuf ();
}
