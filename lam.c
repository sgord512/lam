#include "lam.h"
#include <stdio.h>

void prompt(void);

int main(int argc, char **argv) { 
  puts("Hello, World!");
  prompt();
  return 0;
}

void prompt() {
  char input[1000];    
  fputs(">? ", stdout);
  fgets(input, 1000, stdin);
  fputs(input, stdout);
  return;
}
