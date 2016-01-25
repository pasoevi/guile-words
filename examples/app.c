#include <stdlib.h>
#include <libguile.h>

char *program_name = "dict";

void print_usage(void){
  printf("Usage: \n%s action phrase \n\t where action is one of \
antonym synonym hyphenation prununciation\n", program_name);
}

int main(int argc, char *argv[]){
  SCM func_symbol;
  SCM func;
  SCM ret_val;
  char *action;

  char *word;
  if(argc != 3){
    print_usage();
    exit(EXIT_SUCCESS);
  }else{
    action = argv[1];
    word = argv[2];
  }

  scm_init_guile();

  scm_c_primitive_load ("../words.scm");

  func_symbol = scm_c_lookup(action);
  func = scm_variable_ref(func_symbol);
  ret_val = scm_call_1(func, scm_from_locale_string(word));

  /* SCM is_list = scm_list_p (ret_val); */

  int i, length;
  length = scm_to_int(scm_length (ret_val));

  /* Start from 1 as the zero-th element only denotes query type */
  for(i = 1; i < length; i++){
    SCM elm = scm_list_ref(ret_val, scm_from_int(i));
    char *anton = scm_to_locale_string (elm);
    printf("%s ", anton);
  }
  printf("\n");

  return EXIT_SUCCESS;
}
