#include <stdint.h>

void foo (long a,
          long b,
          long c,
          long d,
          long e,
          long f,
          long g)
{
  c = -b;
}

void main()
{
  foo (0x1234567812345678,
       0x1234567812345678,
       0x1234567812345678,
       0x1234567812345678,
       0x1234567812345678,
       0x1234567812345678,
       0x1234567812345678);
}
