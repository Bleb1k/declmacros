#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#decl(byte_bitmap)
typedef struct Foo {
  int a;
  float *b;
  double *c;
  bool d;
  intptr_t e;
} Foo;
// generates:
/*
typedef void* Byte_Bitmap;
Byte_Bitmap Foo_byte_bitmap = NULL;
// later in main:
{
  Foo i0 = {0};
  int i1 = 0;
  i1 += (sizeof(i0.a)) % 8;
  foo_byte_bitmap |= ((1 << (sizeof(i0.b) % 8)) - 1) << i1;
  // i1 += (sizeof(i0.b) + sizeof(i0.c)) % 8; // would be omitted since no more pointers encountered
}
*/

int main() {
  printf("huh?: 0x%llx\n", Foo_byte_bitmap);
  return 0;
}

