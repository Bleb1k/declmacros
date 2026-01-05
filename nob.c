#include <unistd.h>

#define NOB_STRIP_PREFIX
#define NOB_IMPLEMENTATION
#include "deps/nob.h"

#define FLAG_IMPLEMENTATION
#include "deps/flag.h"

#define BUILD_DIR "build/"
#define GENERATED_DIR BUILD_DIR "generated/"
#define EXAMPLES_DIR "examples/"
#define DEPS_DIR "deps/"
#define SRC_DIR "src/"
#define APP_NAME "cc1"

Cmd *cmd = &(Cmd){0};

#define try_run(...)                                                           \
  if (!cmd_run(cmd, __VA_ARGS__))                                              \
  return 1

bool run_example(const char *name) {
  const char *c_name = temp_sprintf(EXAMPLES_DIR "%s.c", name);
  const char *exe_name = temp_sprintf(BUILD_DIR EXAMPLES_DIR "%s", name);
#if 0
  const char *decl_name = temp_sprintf(GENERATED_DIR "decl.%s.c", name);
  cmd_append(cmd, BUILD_DIR APP_NAME);
  cmd_append(cmd, "-o", decl_name);
  cmd_append(cmd, c_name);
  if (!cmd_run(cmd)) return false;
  
  nob_cc(cmd);
  nob_cc_flags(cmd);
  nob_cc_inputs(cmd, decl_name);
  nob_cc_output(cmd, exe_name);
  if (!cmd_run(cmd)) return false;
#else
  // or try this (for supposed compile_commands.json compatibility (doesn't work for me))
  cmd_append(cmd, "gcc");
  cmd_append(cmd, "-no-integrated-cpp", temp_sprintf("-B%s/" BUILD_DIR, nob_get_current_dir_temp()));
  cmd_append(cmd, "-o", temp_sprintf(BUILD_DIR EXAMPLES_DIR "%s", name));
  cmd_append(cmd, temp_sprintf(EXAMPLES_DIR "%s.c", name));
  if (!cmd_run(cmd)) return false;
#endif
  cmd_append(cmd, temp_sprintf(BUILD_DIR EXAMPLES_DIR "%s", name));
  if (!cmd_run(cmd)) return false;
  return true;
}

int main(int argc, char **argv) {
  NOB_GO_REBUILD_URSELF_PLUS(argc, argv, DEPS_DIR "flag.h");

  bool *release = flag_bool("release", false, "enable optimizations");
  bool *run = flag_bool("run", false, "run the main app");
  bool *run_examples = flag_bool(
      "run-examples", false, "compile and run _example.c files");
  bool *debug = flag_bool("d", false, NULL);
  flag_bool_var(debug, "debug", false, "add debug information");
  bool *clean = flag_bool("c", false, NULL);
  flag_bool_var(clean, "clean", false, "clean artifacts");
  bool *help = flag_bool("h", false, NULL);
  flag_bool_var(help, "help", false, "show this help message");

  if (!flag_parse(argc, argv)) {
    flag_print_error(stderr);
    return 1;
  }

  if (*help) {
    printf("Usage: %s [options]\nOptions:\n", flag_program_name());
    flag_print_options(stdout);
    return 0;
  };

  if (*clean) {
    cmd_append(cmd, "rm", "-r");
    cmd_append(cmd,
      BUILD_DIR,
      "nob",
      "nob.old"
    );
    cmd_run(cmd);
    return 0;
  }

  if (!mkdir_if_not_exists(BUILD_DIR)) return 1;

  nob_cc(cmd);
  nob_cc_flags(cmd);
  cmd_append(cmd, "-I" DEPS_DIR);
  nob_cc_inputs(cmd, SRC_DIR "main.c");
  nob_cc_output(cmd, BUILD_DIR APP_NAME);
  if (*debug) cmd_append(cmd, "-ggdb3", "-O0");
  if (!*debug && *release) cmd_append(cmd, "-O3");
  try_run();

  if (*run) {
    if (*debug) cmd_append(cmd, "gf2", "--args");
    cmd_append(cmd, BUILD_DIR APP_NAME);
    da_append_many(cmd, flag_rest_argv(), flag_rest_argc());
    try_run();
  }

  if (*run_examples) {
    if (!mkdir_if_not_exists(GENERATED_DIR)) return 1;
    if (!mkdir_if_not_exists(BUILD_DIR EXAMPLES_DIR)) return 1;

    if (!run_example("debug_example")) return 1;
    if (!run_example("byte_bitmap_example")) return 1;
  }

  return 0;
}
