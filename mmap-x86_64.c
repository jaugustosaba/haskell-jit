#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

typedef int (*JitFun)(void);

int runJit(char *mem, size_t size) {
  int result = 0;
  void *exec_mem = mmap(NULL, size, PROT_WRITE | PROT_READ | PROT_EXEC,
    MAP_PRIVATE | MAP_ANON, 0, 0);
  if (exec_mem != MAP_FAILED) {
    memcpy(exec_mem, mem, size);
    JitFun fun = (JitFun) exec_mem;
    result = fun();
    if (munmap(exec_mem, size) == -1) {
      perror("runJit fatal error");
      exit(EXIT_FAILURE);
    }
  }
  return result;
}
