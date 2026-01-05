#include <stdio.h>

#define PARSER_IMPLEMENTATION
#include "parser.h"

#define NOB_IMPLEMENTATION
#include "nob.h"

#define FLAG_IMPLEMENTATION
#include "flag.h"

#define Tokens                                            \
  X(invalid)                                              \
                                                          \
  X(identifier)                                           \
  X(macro_identifier)                                     \
  X(number_literal)                                       \
  X(punctuation)                                          \
  X(math_op)                                              \
  X(logical_op)                                           \
  X(comparison)                                           \
  X(deref)                                                \
  X(assignment)                                           \
  X(include_string)                                       \
  X(string_literal)                                       \
  X(char_literal)                                         \
  X(builtin)                                              \
                                                          \
  X(kw_typedef)                                           \
  X(kw_struct)                                            \
  X(kw_enum)                                              \
  X(kw_union)                                             \
  X(kw_return)                                            \
  X(kw_if)                                                \
  X(kw_for)                                               \
  X(kw_while)                                             \
  X(kw_do)                                                \
  X(kw_sizeof)                                            \
                                                          \
  X(eof)

typedef enum TokenKind {
  #define X(name) TOKEN_KIND_(name),
  Tokens
  #undef X
} TokenKind;

const char *TokenKindStr(TokenKind t) {
  switch (t) {
  #define X(name) EXPAND_TOKEN_KIND_STRING(name)
  Tokens
  #undef X
  }
  return nob_temp_sprintf("<unknown %d>", t);
}

typedef struct Token {
  TokenKind kind;
  ParseSlice slice;
} Token;

void dump_token(Token t) {
  printf("`%.*s` (%s)\n",
         t.slice.count > 25 ? 25 : (int)t.slice.count,
         (char*)t.slice.data,
         TokenKindStr(t.kind));
}

bool next_token(ParseSlice *sv, Token *token) {
  ParserStart(char);

  STATE(start,
        SKIP(JUST(' ') || JUST('\n') || JUST('\t') || JUST('\r'), start) else
        TAKE(RANGEi('a', 'z') || RANGEi('A', 'Z') || JUST('_'),
             identifier) else
        SKIP(JUST('\\'), saw_backslash) else
        TAKE(JUST('#'), macro) else
        CONTINUE(atoms));
  STATE(atoms,
        TAKE(JUST('0'), number_choice) else
        TAKE(RANGEi('1', '9'), number) else
        TAKE(JUST('\''), char_literal) else
        TAKE(JUST('"'), string_literal) else
        CONTINUE(punctuation));
  STATE(punctuation,
        TAKE_RETURN(JUST('(') || JUST(')') || JUST('{') || JUST('}') ||
                    JUST('[') || JUST(']') || JUST(';') || JUST(',') ||
                    JUST('?') || JUST(':') || JUST('.'),
             punctuation) else
        TAKE(JUST('<'), saw_less_than) else
        WHEN(JUST('>') || JUST('!'),
             SELECT(comparison)
             TAKE(1, expect_equals_cmp)) else
        WHEN(JUST('='),
             SELECT(assignment)
             TAKE(1, expect_equals_cmp)) else
        CONTINUE(math));
  STATE(math,
        TAKE(JUST('-'), saw_minus) else
        TAKE(JUST('+'), saw_plus) else
        TAKE(JUST('|'), expect_or) else
        TAKE(JUST('&'), expect_and) else
        WHEN(JUST('*'),
             SELECT(math_op)
             TAKE(1, expect_equals_cmp)) else
        CONTINUE(math_2));
  STATE(math_2,
        TAKE(JUST('/'), saw_slash) else
        WHEN(JUST('%'),
             SELECT(math_op)
             TAKE(1, expect_equals)) else
        CONTINUE(end));
  STATE(number_choice,
        SKIP(JUST('x') || JUST('X'), hex_number) else
        SKIP(JUST('o') || JUST('O'), octal_number) else
        SKIP(JUST('b') || JUST('B'), binary_number) else
        SELECT(number_literal));
  STATE(number,
        SKIP(RANGEi('0', '9'), number) else
        SKIP(JUST('.'), floating_number) else
        SELECT(number_literal));
  STATE(floating_number, NOB_TODO("Floating number support."));
  STATE(hex_number,
        SKIP(RANGEi('0', '9') || RANGEi('a', 'f'), binary_number));
  STATE(octal_number,
        SKIP(RANGEi('0', '7'), binary_number));
  STATE(binary_number,
        SKIP(JUST('0') || JUST('1'), binary_number));
  STATE(identifier,
        SKIP(RANGEi('a', 'z') || RANGEi('A', 'Z') || JUST('_') || RANGEi('0', '9'),
             identifier) else
        SELECT(identifier)
        CONTINUE(detect_keywords));
  STATE(macro,
        SKIP(JUST(' '), macro) else
        RETURN(JUST('\n'), invalid) else
        SKIP(RANGEi('a', 'z') || RANGEi('0', '9'), macro_identifier) else
        RETURN(1, invalid));
  STATE(macro_identifier,
        SKIP(RANGEi('a', 'z'), macro_identifier) else
        SELECT(macro_identifier));
  STATE(detect_keywords,
        token->slice.count -= count;
        WHEN(STR("typedef"), SELECT(kw_typedef)) else
        WHEN(STR("struct"), SELECT(kw_struct)) else
        WHEN(STR("enum"), SELECT(kw_enum)) else
        WHEN(STR("union"), SELECT(kw_union)) else
        WHEN(STR("return"), SELECT(kw_return)) else
        CONTINUE(detect_keywords_2);
        token->slice.count += count);
  STATE(detect_keywords_2,
        WHEN(STR("sizeof"), SELECT(kw_sizeof)) else
        WHEN(STR("if"), SELECT(kw_if)) else
        WHEN(STR("for"), SELECT(kw_for)) else
        WHEN(STR("while"), SELECT(kw_while)) else
        WHEN(STR("do"), SELECT(kw_do));
        token->slice.count += count);
  STATE(string_literal,
        RETURN(JUST('"'), string_literal) else
        SKIP(JUST('\\'), string_literal_backslash) else
        RETURN(JUST('\n'), invalid) else
        SKIP(1, string_literal));
  STATE(string_literal_backslash,
        RETURN(JUST('\0') || JUST('\n'), invalid) else
        SKIP(1, string_literal));
  STATE(char_literal,
        RETURN(JUST('\''), char_literal) else
        SKIP(JUST('\\'), char_literal_backslash) else
        RETURN(JUST('\n'), invalid) else
        RETURN(JUST('\0') || JUST('\n') || JUST(127) || RANGEi(1, 9) || RANGEi(11, 31),
             invalid) else
        SKIP(1, char_literal));
  STATE(char_literal_backslash,
        RETURN(JUST('\0') || JUST('\n') || JUST(127) || RANGEi(1, 9) || RANGEi(11, 31),
             invalid) else
        SKIP(1, char_literal));
  STATE(saw_minus,
        SKIP(JUST('-'), end_ok) else
        RETURN(JUST('>'), deref) else
        RETURN(JUST('='), assignment) else
        SELECT(math_op));
  STATE(saw_plus,
        SKIP(JUST('+'), end_ok) else
        RETURN(JUST('='), assignment) else
        SELECT(math_op));
  STATE(saw_slash,
        SKIP(JUST('/'), single_line_comment) else
        SKIP(JUST('*'), multi_line_comment) else
        RETURN(JUST('='), assignment) else
        SELECT(math_op));
  STATE(saw_backslash,
        SKIP(JUST(' ') || JUST('\r'), saw_backslash) else
        SKIP(JUST('\n'), start) else
        RETURN(1, invalid));
  STATE(saw_less_than,
        WHEN(JUST(' ') || JUST('\n') || JUST('\t') || JUST('\r'),
             SELECT(math_op)
             CONTINUE(end_ok))
        CONTINUE(include_string));
  STATE(include_string,
        RETURN(JUST('\0') || JUST('\n'), invalid) else
        RETURN(JUST('>'), include_string) else
        SKIP(1, include_string));
  STATE(expect_or,
        WHEN(JUST('|'),
             SELECT(logical_op)
             SKIP(1, expect_equals)) else
        SELECT(math_op)
        CONTINUE(expect_equals));
  STATE(expect_and,
        WHEN(JUST('&'),
             SELECT(logical_op)
             SKIP(1, expect_equals))
        SELECT(math_op)
        CONTINUE(expect_equals));
  STATE(expect_equals,     RETURN(JUST('='), assignment));
  STATE(expect_equals_cmp, RETURN(JUST('='), comparison));
  STATE(single_line_comment,
        SKIP(JUST('\n'), start) else
        SKIP(1, single_line_comment));
  STATE(multi_line_comment,
        SKIP(JUST('*'), end_multi_line_comment) else
        SKIP(1, multi_line_comment));
  STATE(end_multi_line_comment,
        SKIP(JUST('/'), start) else
        CONTINUE(multi_line_comment));

  ParserEnd();
  // dump_token(*token);
  return result;
}

#define THIS_FILE __FILE__
#define THIS_LINE __LINE__
#define cstr(str) (str), (sizeof(str) - 1)

#define TRY_NEXT_TOKEN()                                                       \
  if (!next_token(&src, &tok)) {                                               \
    nob_log(NOB_ERROR, "Unexpected EOF");                                      \
    abort();                                                                   \
  }

#define REWIND_TOKEN() src.count += tok.slice.count; src.data -= tok.slice.count

#define EXPECT_KIND_AND_STR(kind_, str_)                                       \
  if (                                                                         \
    tok.kind != TOKEN_KIND_(kind_)                                             \
    || !ParseSlice_cstr_cmp(tok.slice, cstr(str_))                             \
  ) {                                                                          \
    nob_log(                                                                   \
      NOB_ERROR,                                                               \
      __FILE__ ":%d Expected " #kind_ " " str_ ", got %s `%.*s`",              \
      __LINE__,                                                                \
      TokenKindStr(tok.kind),                                                  \
      (int)tok.slice.count,                                                    \
      (char*)tok.slice.data                                                    \
    );                                                                         \
    abort();                                                                   \
  }
#define EXPECT_KIND_AND_STR_OPT(kind_, str_)                                   \
  if (                                                                         \
    tok.kind != TOKEN_KIND_(kind_)                                             \
    || !ParseSlice_cstr_cmp(tok.slice, cstr(str_))                             \
  ) {                                                                          \
    REWIND_TOKEN();                                                            \
  }
#define EXPECT_KIND_AND_STR_OR_RET(kind_, str_, ret_)                          \
  if (                                                                         \
    tok.kind != TOKEN_KIND_(kind_)                                             \
    || !ParseSlice_cstr_cmp(tok.slice, cstr(str_))                             \
  ) {                                                                          \
    nob_log(                                                                   \
      NOB_ERROR,                                                               \
      __FILE__ ":%d Expected " #kind_ " " str_ ", got %s `%.*s`",              \
      __LINE__,                                                                \
      TokenKindStr(tok.kind),                                                  \
      (int)tok.slice.count,                                                    \
      (char*)tok.slice.data                                                    \
    );                                                                         \
    return ret_;                                                               \
  }

#define EXPECT_KIND(kind_)                                                     \
  if (                                                                         \
    tok.kind != TOKEN_KIND_(kind_)                                             \
  ) {                                                                          \
    nob_log(                                                                   \
      NOB_ERROR,                                                               \
      __FILE__ ":%d Expected " #kind_ ", got %s `%.*s`",                       \
      __LINE__,\
      TokenKindStr(tok.kind),                                                  \
      (int)tok.slice.count,                                                    \
      (char*)tok.slice.data                                                    \
    );                                                                         \
    abort();                                                                   \
  }
#define EXPECT_KIND_OPT(kind_)                                                 \
  if (tok.kind != TOKEN_KIND_(kind_)) { REWIND_TOKEN(); }
#define EXPECT_KIND_OR_RET(kind_, ret_)                                        \
  if (tok.kind != TOKEN_KIND_(kind_)) {                                        \
    nob_log(                                                                   \
      NOB_ERROR,                                                               \
      __FILE__ ":%d Expected " #kind_ ", got %s `%.*s`",                       \
      __LINE__,\
      TokenKindStr(tok.kind),                                                  \
      (int)tok.slice.count,                                                    \
      (char*)tok.slice.data                                                    \
    );                                                                         \
    return ret_;                                                               \
  }

#define TRY_NEXT_AND_EXPECT_KIND(kind_) TRY_NEXT_TOKEN(); EXPECT_KIND(kind_);
#define TRY_NEXT_AND_EXPECT_KIND_OPT(kind_) TRY_NEXT_TOKEN(); EXPECT_KIND_OPT(kind_);
#define TRY_NEXT_AND_EXPECT_KIND_OR_RET(kind_, ret_) TRY_NEXT_TOKEN(); EXPECT_KIND_OR_RET(kind_, ret_);
#define TRY_NEXT_AND_EXPECT_KIND_AND_STR(kind_, str_) TRY_NEXT_TOKEN(); EXPECT_KIND_AND_STR(kind_, str_);
#define TRY_NEXT_AND_EXPECT_KIND_AND_STR_OPT(kind_, str_) TRY_NEXT_TOKEN(); EXPECT_KIND_AND_STR_OPT(kind_, str_);
#define TRY_NEXT_AND_EXPECT_KIND_AND_STR_OR_RET(kind_, str_, ret_) TRY_NEXT_TOKEN(); EXPECT_KIND_AND_STR_OR_RET(kind_, str_, ret_);

typedef struct DefItem {
  ParseSlice name, type;
  bool pointer;
} DefItem;

typedef struct DefItems {
  DefItem *items;
  size_t count, capacity;
} DefItems;

typedef struct StructAst {
  ParseSlice src;
  DefItems items;
} StructAst;

typedef struct TypedefAst {
  ParseSlice src, name;
  enum {
    TDKind_invalid,
    TDKind_struc,
    TDKind_alias,
  } kind;
  union {
    StructAst struc;
    ParseSlice ident;
  };
} TypedefAst;

typedef struct TypedefAsts {
  TypedefAst *items;
  size_t count, capacity;
} TypedefAsts;

typedef struct DeclAst {
  ParseSlice src;
  enum {
    DECLKind_debug       = 1 << 0,
    DECLKind_byte_bitmap = 1 << 1,
  } kinds;
} DeclAst;

bool parse_struct(ParseSlice *src_p, StructAst *out) {
  ParseSlice src = *src_p;
  Token tok = {0};
  TRY_NEXT_AND_EXPECT_KIND_OR_RET(kw_struct, false);
  TRY_NEXT_AND_EXPECT_KIND_OPT(identifier);
  if (tok.kind == TokenKind_identifier) {
    *(ParseSlice*)(void*)out = tok.slice;
  }
  TRY_NEXT_AND_EXPECT_KIND_AND_STR_OPT(punctuation, "{");
  if (tok.kind != TokenKind_punctuation) {
    ((TypedefAst*)(void*)((char*)(void*)out - 40))->kind = TDKind_alias;
  } else {
    TRY_NEXT_TOKEN()
    while (tok.kind != TokenKind_punctuation || !ParseSlice_cstr_cmp(tok.slice, cstr("}"))) {
      DefItem it = {0};
      if (tok.kind != TokenKind_identifier) {
        fprintf(stderr, "decl only support structs with types with 0 or 1 indirection levels: `type [*] name`\n");
        fprintf(stderr, "typ; unsupported: `%.*s` (%s)\n", tok.slice.count > 25 ? 25 : (int)tok.slice.count, (char*)tok.slice.data, TokenKindStr(tok.kind));
        return false;
      }
      it.type = tok.slice;
      TRY_NEXT_TOKEN();
      if (tok.kind == TokenKind_math_op) {
        if (!ParseSlice_cstr_cmp(tok.slice, cstr("*"))) {
          fprintf(stderr, "ptr; unexpected ptr symbol: `%.*s`, expected `*`", (int)tok.slice.count, (char*)tok.slice.data);
          return false;
        }
        it.pointer = true;
        TRY_NEXT_TOKEN();
      }
      if (tok.kind != TokenKind_identifier) {
        fprintf(stderr, "decl only support structs with types with 0 or 1 indirection levels: `type [*] name`\n");
        fprintf(stderr, "nam; unsupported: `%.*s` (%s)\n", tok.slice.count > 25 ? 25 : (int)tok.slice.count, (char*)tok.slice.data, TokenKindStr(tok.kind));
        return false;
      }
      it.name = tok.slice;
      TRY_NEXT_TOKEN();
      if (
        tok.kind != TokenKind_punctuation
        || !ParseSlice_cstr_cmp(tok.slice, cstr(";"))
      ) {
        fprintf(stderr, "decl only support structs with types with 0 or 1 indirection levels: `type [*] name`\n");
        fprintf(stderr, "pun; unsupported: `%.*s` (%s)\n", tok.slice.count > 25 ? 25 : (int)tok.slice.count, (char*)tok.slice.data, TokenKindStr(tok.kind));
        return false;
      }
      nob_da_append(&out->items, it);
      TRY_NEXT_TOKEN();
    }
    out->src.data = src_p->data;
    out->src.count = src.data - src_p->data;
  }
  *src_p = src;
  return true;
}

bool parse_typedef(ParseSlice *src_p, TypedefAst *out) {
  ParseSlice src = *src_p;
  Token tok = {0};
  TRY_NEXT_AND_EXPECT_KIND_OR_RET(kw_typedef, false);
  TRY_NEXT_TOKEN();
  switch (tok.kind) {
    case TokenKind_kw_struct: 
      REWIND_TOKEN();
      if (!parse_struct(&src, &out->struc)) {
        fprintf(stderr, "didn't parse struct lmao\n");
        return false;
      }
      if (!out->kind) out->kind = TDKind_struc;
      break;
    case TokenKind_identifier:
      out->ident = tok.slice;
      out->kind = TDKind_alias;
      break;
    default:
      printf("unsupported: `%.*s` (%s)\n", tok.slice.count > 25 ? 25 : (int)tok.slice.count, (char*)tok.slice.data, TokenKindStr(tok.kind));
      return false;
  }
  TRY_NEXT_AND_EXPECT_KIND_OR_RET(identifier, false);
  out->name = tok.slice;
  TRY_NEXT_AND_EXPECT_KIND_AND_STR_OR_RET(punctuation, ";", false);
  out->src.data = src_p->data;
  out->src.count = src.data - src_p->data;
  *src_p = src;
  return true;
}

bool parse_decl(ParseSlice *src_p, DeclAst *out) {
  ParseSlice src = *src_p;
  Token tok = {0};
  TRY_NEXT_AND_EXPECT_KIND_AND_STR_OR_RET(macro_identifier, "#decl", false);
  TRY_NEXT_AND_EXPECT_KIND_AND_STR_OR_RET(punctuation, "(", false);
  for (;;) {
    TRY_NEXT_AND_EXPECT_KIND_OR_RET(identifier, false);
    if (ParseSlice_cstr_cmp(tok.slice, cstr("debug"))) {
      out->kinds |= DECLKind_debug;
    } else if (ParseSlice_cstr_cmp(tok.slice, cstr("byte_bitmap"))) {
      out->kinds |= DECLKind_byte_bitmap;
    } else {
      nob_log(NOB_ERROR, "unexpected decl macro: %.*s", (int)tok.slice.count, (char*)tok.slice.data);
      abort();
    }
    TRY_NEXT_TOKEN();
    if (tok.kind != TokenKind_punctuation || !ParseSlice_cstr_cmp(tok.slice, cstr(","))) {
      REWIND_TOKEN();
      break;
    }
  }
  TRY_NEXT_AND_EXPECT_KIND_AND_STR_OR_RET(punctuation, ")", false);
  out->src.data = src_p->data;
  out->src.count = src.data - src_p->data;
  *src_p = src;
  return true;
}

typedef struct TypePrinter {
  enum {
    TypePrKind_invalid,
    TypePrKind_simple,
    TypePrKind_struct,
  } kind;
  ParseSlice from;
  const char *to;
} TypePrinter;

typedef struct TypePrinters {
  TypePrinter *items;
  size_t count, capacity;
} TypePrinters;

TypePrinters types = {0};

TypePrinter type_slice_to_cstr_label(ParseSlice typ) {
  nob_da_foreach(TypePrinter, it, &types) {
    if (ParseSlice_cstr_cmp(it->from, typ.data, typ.count)) {
      return *it;
    }
  }
  fprintf(stderr, "heck, unknown type (%.*s) :(", (int)typ.count, (char*)typ.data);
  return (TypePrinter){TypePrKind_simple, {0}, nob_temp_sprintf("%%(unknown %.*s)", (int)typ.count, (char*)typ.data)};
}

int main(int argc, char **argv) {
  bool help;
  flag_bool_var(&help, "h", false, NULL);
  flag_bool_var(&help, "help", false, "show this help message");
  char **output = flag_str("o", NULL, "output file path");
  char *input_file_path = NULL;
  
  bool *force_cpp = flag_bool("E", false, "forces cpp to run after this, taking all cc flags");
  (void)flag_bool("quiet", false, "cc flag");
  char **mtune = flag_str("mtune", NULL, "cc flag");
  char **march = flag_str("march", NULL, "cc flag");
  bool *preprocessed = flag_bool("fpreprocessed", false, "cc flag");
  char **dumpdir = flag_str("dumpdir", NULL, "cc flag");
  char **dumpbase = flag_str("dumpbase", NULL, "cc flag");
  char **dumpbase_ext = flag_str("dumpbase-ext", NULL, "cc flag");
  
  if (!flag_parse(argc, argv)) {
    flag_print_error(stderr);
    return 1;
  }
  
  argc = flag_rest_argc();
  argv = flag_rest_argv();
  if (argc > 0) input_file_path = flag_shift_args(&argc, &argv);

  if (!flag_parse(argc, argv)) {
    flag_print_error(stderr);
    return 1;
  }
  argc = flag_rest_argc();
  argv = flag_rest_argv();

  if (help) {
    fprintf(stdout, "Usage: %s [options] file_path\nOptions:\n", flag_program_name());
    flag_print_options(stdout);
    return 0;
  };

  if (argc > 0) {
    fprintf(stderr, "Error: malformed input\n");
    fprintf(stderr, "Usage: %s [options] input_file\nOptions:\n", flag_program_name());
    flag_print_options(stderr);
    abort();
  }

  nob_minimal_log_level = NOB_ERROR;

  if (*preprocessed) {
    Nob_Cmd cmd = {0};
    nob_cmd_append(&cmd, "cc", "-fpreprocessed", "-S");
    nob_cmd_append(&cmd, input_file_path);
    if (*dumpdir != NULL) nob_cmd_append(&cmd, "-dumpdir", *dumpdir);
    if (*dumpbase != NULL) nob_cmd_append(&cmd, "-dumpbase", *dumpbase);
    if (*dumpbase_ext != NULL) nob_cmd_append(&cmd, "-dumpbase-ext", *dumpbase_ext);
    if (mtune != NULL) nob_cmd_append(&cmd, nob_temp_sprintf("-mtune=%s", *mtune));
    if (march != NULL) nob_cmd_append(&cmd, nob_temp_sprintf("-march=%s", *march));
    if (march != NULL) nob_cmd_append(&cmd, "-o", *output);
    if (!nob_cmd_run(&cmd)) return 1;
    return 0;
  }

  Nob_String_Builder input = {0};
  ParseSlice src = {0};
  if (!nob_read_entire_file(input_file_path, &input)) return 1;
  src.data = input.items;
  src.count = input.count;

  TypePrinter known_types[] = {
    (TypePrinter){TypePrKind_simple, {cstr("int")}, "d"},
    (TypePrinter){TypePrKind_simple, {cstr("float")}, "f"}
  };
  
  nob_da_append_many(&types, known_types, NOB_ARRAY_LEN(known_types));

  bool byte_bitmap_typedef_exists = false;
  TypedefAsts init_byte_bitmap_targets = {0};

  Nob_String_Builder out = {0};
  Token tok = {0};
  while (next_token(&src, &tok)) {
    switch (tok.kind) {
    case TokenKind_macro_identifier: {
      if (ParseSlice_cstr_cmp(tok.slice, cstr("#decl"))) {
        REWIND_TOKEN();
        DeclAst decl = {0};
        if (!parse_decl(&src, &decl)) abort();
        TypedefAst td = {0};
        if (!parse_typedef(&src, &td)) abort();
        const char *typename = nob_temp_sv_to_cstr((Nob_String_View){td.name.count, td.name.data});
        
        nob_sb_append_buf(&out, td.src.data, td.src.count);
        if (decl.kinds & DECLKind_debug) {
          switch (td.kind) {
          case TDKind_struc:
            nob_sb_appendf(&out,
              "\nstatic inline void %s__debug(%s it) { ",
              typename, typename
            );
            nob_da_foreach(DefItem, it, &td.struc.items) {
              const char* itname = nob_temp_sv_to_cstr((Nob_String_View){it->name.count, it->name.data});
              TypePrinter ittype = type_slice_to_cstr_label(it->type);
              switch (ittype.kind) {
              case TypePrKind_simple:
                nob_sb_appendf(&out,
                  " fprintf(stdout, \"%s%s: %%%s\", it.%s);",
                  ((void*)it != td.struc.items.items) ? ", " : "", itname, ittype.to, itname
                );
                break;
              case TypePrKind_struct:
                nob_sb_appendf(&out,
                  " fprintf(stdout, \"%s%s: \");",
                  ((void*)it != td.struc.items.items) ? ", " : "", itname
                );
                nob_sb_appendf(&out,
                  " %.*s_debug(it.%s);",
                  (int)ittype.from.count, (char*)ittype.from.data, itname
                );
                break;
              default: NOB_UNREACHABLE("Invalid TypePrinter kind");
              }
            }
            nob_sb_append_cstr(&out, " }\n");
            nob_sb_appendf(&out,
              "void %s_debug(%s it) { fprintf(stdout, \"%s{ \"); %s__debug(it); fprintf(stdout, \" }\"); }\n",
              typename, typename, typename, typename
            );
            break;

          case TDKind_alias:
            nob_sb_appendf(&out,
              "\nvoid %s_debug(%s it) { fprintf(stdout, \"%s{ \"); %.*s__debug(it); fprintf(stdout, \" }\"); }\n",
              typename, typename, typename, (int)td.ident.count, (char*)td.ident.data
            );
            break;

          default: NOB_UNREACHABLE("unhandled typedef kind");
          }
          nob_da_append(&types, ((TypePrinter){TypePrKind_struct, td.name, NULL}));
        }
        if (decl.kinds & DECLKind_byte_bitmap) {
          if (!byte_bitmap_typedef_exists) {
            byte_bitmap_typedef_exists = true;
            nob_sb_appendf(&out, "\ntypedef unsigned long long int Byte_Bitmap;\n");
          }
          nob_sb_appendf(&out, "Byte_Bitmap %s_byte_bitmap = 0;\n", typename);
          nob_da_append(&init_byte_bitmap_targets, td);
        }
        continue;
      }
      nob_da_append(&out, '\n');
      break;
    }
    case TokenKind_identifier: {
      if (!ParseSlice_cstr_cmp(tok.slice, cstr("main"))) break;
      nob_sb_append_buf(&out, tok.slice.data, tok.slice.count);
      nob_da_append(&out, ' ');
      TRY_NEXT_AND_EXPECT_KIND(punctuation);
      if (!ParseSlice_cstr_cmp(tok.slice, cstr("("))) {
        nob_log(NOB_ERROR, THIS_FILE ":%d expected `(`, got `%.*s`", THIS_LINE, (int)tok.slice.count, (char*)tok.slice.data);
        abort();
      }
      
      for (int depth = 1; depth;) {
        nob_sb_append_buf(&out, tok.slice.data, tok.slice.count);
        nob_da_append(&out, ' ');
        TRY_NEXT_TOKEN();
        
        if (tok.kind != TokenKind_punctuation) continue;
        if (ParseSlice_cstr_cmp(tok.slice, cstr("("))) depth += 1;
        else if (ParseSlice_cstr_cmp(tok.slice, cstr(")"))) depth -= 1;
      }
      nob_sb_append_buf(&out, tok.slice.data, tok.slice.count);
      nob_da_append(&out, ' ');
      TRY_NEXT_AND_EXPECT_KIND_AND_STR(punctuation, "{");
      nob_da_append(&out, '{');
      if (init_byte_bitmap_targets.count > 0) {
        nob_sb_append_cstr(&out, "\n#define __BB_SIZE__(n_) ((((sizeof(i0. n_) % sizeof(void*)) ? 1 : 0) + sizeof(i0. n_) / sizeof(void*)) * sizeof(void*))\n");
        nob_da_foreach(TypedefAst, td, &init_byte_bitmap_targets) {
          const char *typename = nob_temp_sv_to_cstr((Nob_String_View){td->name.count, td->name.data});
          char *temp_accumulator = "";

          nob_sb_appendf(&out,
            "  {\n"
            "    %s i0 = {0};\n"
            "    int i1 = 0;\n",
            typename);
          nob_da_foreach(DefItem, it, &td->struc.items) {
            if (it->pointer) {
              if (*temp_accumulator) nob_sb_appendf(&out, "    i1 += %s;\n", temp_accumulator);
              nob_sb_appendf(&out,
                "    %s_byte_bitmap |= ((1 << sizeof(i0.%.*s)) - 1) << i1;\n",
                typename,
                (int)it->name.count,
                (char*)it->name.data
              );
              temp_accumulator = "";
            }
            temp_accumulator = nob_temp_sprintf(
              "%s%s__BB_SIZE__(%.*s)",
              temp_accumulator,
              !*temp_accumulator ? "" : " + ",
              (int)it->name.count,
              (char*)it->name.data
            );
          }
          nob_sb_append_cstr(&out, "  }\n");
        }
        nob_sb_append_cstr(&out, "#undef __BB_SIZE__\n");
      }
      continue;
    }
    case TokenKind_include_string:
      nob_sb_append_buf(&out, tok.slice.data, tok.slice.count);
      nob_da_append(&out, '\n');
      continue;
    default:
      break;
    }
    nob_sb_append_buf(&out, tok.slice.data, tok.slice.count);
    nob_da_append(&out, ' ');
  }

  // -E provided
  if (*force_cpp) {
    char *intermediate_out_file = nob_temp_sprintf("%s.c", *output);
    if (!nob_write_entire_file(intermediate_out_file, out.items, out.count)) return 1;

    Nob_Cmd cmd = {0};
    nob_cmd_append(&cmd, "cpp");
    nob_cmd_append(&cmd, intermediate_out_file);
    if (mtune != NULL) nob_cmd_append(&cmd, nob_temp_sprintf("-mtune=%s", *mtune));
    if (march != NULL) nob_cmd_append(&cmd, nob_temp_sprintf("-march=%s", *march));
    if (march != NULL) nob_cmd_append(&cmd, "-o", *output);
    if (!nob_cmd_run(&cmd)) return 1;
    return 0;
  } else {
    if (*output != NULL) nob_write_entire_file(*output, out.items, out.count);
    else printf("%.*s\n", (int)out.count, out.items);
  }

  return 0;
}
