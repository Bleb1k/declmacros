#include <stdio.h>

#decl(debug)
typedef struct vec2 {int x; int y;} vec2;

#decl(debug)
typedef vec2 pos2d;

#decl(debug)
typedef struct vec2 v2;

#decl(debug)
typedef struct {vec2 r1; vec2 r2;} mat2;

int main() {
  printf("huh?: ");
  vec2_debug((vec2){1, 3});
  printf(";\n");
  pos2d_debug((pos2d){2, 4});
  printf(" lmao;\n");
  v2_debug((v2){2, 4});
  printf("\n");
  mat2_debug((mat2){{0, 1}, {2, 3}});
  printf("\n");
  return 0;
}
