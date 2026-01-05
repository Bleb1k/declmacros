#ifndef PARSER_H_
#define PARSER_H_

#include <stdbool.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

#define STATE(N, expr)                                                          \
  N:;                                                                          \
  if (count <= 0) goto end;                                                    \
  NextItem;                                                                   \
  expr;                                                \
  CONTINUE(end_ok)

// P - parsers - conditions and stuff like that
#define JUST(N) (it == (N))
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
#define WHEN(P, blk) if (P) { blk; }

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

// check if slice is the same as specified str@len
bool ParseSlice_cstr_cmp(ParseSlice slice, const char *str, size_t len) {
  if (slice.count != len) return false;
  for (size_t i = 0; i < len; ++i)
    if (((char *)(slice.data))[i] != str[i]) return false;
  return true;
}

#endif // PARSER_IMPLEMENTATION
#endif // PARSER_IMPLEMENTATION_

