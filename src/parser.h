#ifndef PARSER_H_
#define PARSER_H_

#include <stdbool.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

// ////////////////////////////////////////////////////////////////////////////////////////////////////

// #define PP_NARG(...) PP_NARG_(__VA_ARGS__, PP_RSEQ_N())
// #define PP_NARG_(...) PP_128TH_ARG(__VA_ARGS__)
/*
// #define PP_128TH_ARG(                                                          \
//     _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16,     \
//     _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, \
//     _32, _33, _34, _35, _36, _37, _38, _39, _40, _41, _42, _43, _44, _45, _46, \
//     _47, _48, _49, _50, _51, _52, _53, _54, _55, _56, _57, _58, _59, _60, _61, \
//     _62, _63, _64, _65, _66, _67, _68, _69, _70, _71, _72, _73, _74, _75, _76, \
//     _77, _78, _79, _80, _81, _82, _83, _84, _85, _86, _87, _88, _89, _90, _91, \
//     _92, _93, _94, _95, _96, _97, _98, _99, _100, _101, _102, _103, _104,      \
//     _105, _106, _107, _108, _109, _110, _111, _112, _113, _114, _115, _116,    \
//     _117, _118, _119, _120, _121, _122, _123, _124, _125, _126, _127, N, ...)  \
//   N
// #define PP_RSEQ_N()                                                            \
//   127, 126, 125, 124, 123, 122, 121, 120, 119, 118, 117, 116, 115, 114, 113,   \
//       112, 111, 110, 109, 108, 107, 106, 105, 104, 103, 102, 101, 100, 99, 98, \
//       97, 96, 95, 94, 93, 92, 91, 90, 89, 88, 87, 86, 85, 84, 83, 82, 81, 80,  \
//       79, 78, 77, 76, 75, 74, 73, 72, 71, 70, 69, 68, 67, 66, 65, 64, 63, 62,  \
//       61, 60, 59, 58, 57, 56, 55, 54, 53, 52, 51, 50, 49, 48, 47, 46, 45, 44,  \
//       43, 42, 41, 40, 39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26,  \
//       25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, \
//       6, 5, 4, 3, 2, 1, 0
*/
////////////////////////////////////////////////////////////////////////////////////////////////////

// Actual library
/*
#define STATE(N, ...)                                                          \
  N:;                                                                          \
  if (count <= 0) goto end;                                                    \
  NextItem;                                                                   \
  MAP_ELSE(EVAL0, __VA_ARGS__);                                                \
  CONTINUE(end_ok)
*/
#define STATE(N, expr)                                                          \
  N:;                                                                          \
  if (count <= 0) goto end;                                                    \
  NextItem;                                                                   \
  expr;                                                \
  CONTINUE(end_ok)

// P - parsers - conditions and stuff like that
#define JUST(N) (it == (N))
/*
#define ALT(...) (MAP_OR(CASE_, __VA_ARGS__))
*/
#define RANGE(from, to) (from <= it && it < to)
#define RANGEi(from, to) (from <= it && it <= to)
#define STR(cstr) ParseSlice_cstr_cmp(token->slice, (cstr), sizeof(cstr) - 1)

// State parts
#define CONTINUE(state) goto state;
#define SKIP(P, state)                                                         \
  if (P) {                                                                     \
    ++src;                                                                     \
    --count;                                                                   \
    CONTINUE(state)                                                            \
  }
#define TAKE(P, state)                                                         \
  if (P) {                                                                     \
    token->slice.data = src++;                                                 \
    token->slice.count = count--;                                              \
    CONTINUE(state)                                                            \
  }

#define RETURN(P, T_kind)                                                      \
  if (P) {                                                                     \
    token->kind = PP_CAT(TagPrefix, _##T_kind);                                \
    ++src;                                                                     \
    --count;                                                                   \
    CONTINUE(end_ok)                                                           \
  }
#define TAKE_RETURN(P, T_kind)                                                 \
  if (P) {                                                                     \
    token->kind = PP_CAT(TagPrefix, _##T_kind);                                \
    token->slice.data = src++;                                                 \
    token->slice.count = count--;                                              \
    CONTINUE(end_ok)                                                           \
  }
#define SELECT(T_kind) token->kind = TOKEN_KIND_(T_kind);
#define SEQUENCE()
#define WHEN(P, blk)                                                           \
  if (P) { blk; }
// #define DO(...) {MAP(STATEMENT_, __VA_ARGS__);}
// Internal mechanisms

typedef struct ParsedSlice {
  const void *data;
  size_t count;
} ParseSlice;

bool ParseSlice_cstr_eq(ParseSlice slice, const char *str, size_t len);

// Macro interface

#ifndef NextItem
#define NextItem DefaultNextItem
#endif
#ifndef TagPrefix
#define TagPrefix DefaultTagPrefix
#endif
#define ParserArgs ParseSlice *sv, Token *token
#define ParserStart(ParseItemType)                                             \
  const ParseItemType *src = sv->data;                                         \
  size_t count = sv->count;                                                    \
  ParseItemType it;                                                            \
  bool result = false;                                                         \
  token->kind = 0;                                                             \
  token->slice = (ParseSlice){0}
#define ParserEnd(ignore_sv)                                                   \
end_ok:                                                                        \
  result = true;                                                               \
end:                                                                           \
  token->slice.count -= count;                                                 \
  if ((ignore_sv - 1) < 0) {                                                          \
    sv->data = src;                                                            \
    sv->count = count;                                                         \
  }
#define DefineTokens(...) ENUM_TAG(TokenKind, __VA_ARGS__)                     \
const char *TokenKindStr(TokenKind t) {                                        \
  switch (t) {                                                                 \
    MAP(EXPAND_TOKEN_KIND_STRING, __VA_ARGS__)                                 \
  }                                                                            \
  return "";                                                                   \
}                                                                              \
typedef struct Token {                                                         \
  TokenKind kind;                                                              \
  ParseSlice slice;                                                            \
} Token;                                                                       \
void dump_token(Token t) {\
  printf("`%.*s` (%s)\n", t.slice.count > 25 ? 25 : (int)t.slice.count, (char*)t.slice.data, TokenKindStr(t.kind));\
}

// Macro interface helpers

#define CASE_(T) T
#define STATEMENT_(T) T;
#define TOKEN_KIND_(T) PP_CAT(TagPrefix, _##T)

#define PP_CAT_(l, r) l##r
#define PP_CAT(l, r) PP_CAT_(l, r)

#define EXPAND_TOKEN_KIND_STRING(v) case PP_CAT(TokenKind_, v): return #v;

#define ENUM_TAG(name, ...) typedef enum name {EVAL0(MAP_1ARG_LIST(PP_CAT, name##_, __VA_ARGS__))} name;

#define DefaultNextItem it = *src
#define DefaultTagPrefix TokenKind

#ifdef __cplusplus
}
#endif

#endif // PARSER_H_
#ifndef PARSER_IMPLEMENTATION_
#ifdef PARSER_IMPLEMENTATION
#define PARSER_IMPLEMENTATION_

bool ParseSlice_cstr_cmp(ParseSlice slice, const char *str, size_t len) {
  if (slice.count < len) return false;
  for (size_t i = 0; i < len; ++i)
    if (((char *)(slice.data))[i] != str[i]) return false;
  return true;
}

#endif // PARSER_IMPLEMENTATION
#endif // PARSER_IMPLEMENTATION_

