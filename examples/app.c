#include <stdio.h>
#include <stdbool.h>
#include <errno.h>
#include <stdlib.h>
#include <libguile.h>

char *program_name = "dict";
char *version = "0.01";
char *hist_fname = "/home/sergi/.weehist";

/* Declare SCM types for calling a function and reading its output */
SCM func_symbol;
SCM func;
SCM ret_val;

void print_usage(void){
  printf("Usage: \n%s action phrase \n\t where action is one of \
antonym synonym hyphenation prununciation\n", program_name);
}

void print_version(void){
  printf("%s\n", version);
}

int append_line(char *word, char *history_file){
  int errnum = 0;
  FILE *fp = fopen(history_file, "a");

  if(fp == NULL){
    errnum = errno;
    perror("Error opening the history file: ");
  }else{
    fprintf(fp, "%s\n", word);
    fclose(fp);
  }

  return errnum;
}

int add_to_history(char *word, char *history_file){
  int errnum = append_line(word, history_file);
  return errnum;
}

void process_phrase(char *action,  char *word, bool add_to_hist){
  /* Add to history */
  if(add_to_hist){
    int errnum;
    errnum = add_to_history(word, hist_fname);
  }

  /* Look the word up */
  func_symbol = scm_c_lookup(action);
  func = scm_variable_ref(func_symbol);
  ret_val = scm_call_1(func, scm_from_locale_string(word));

  /* SCM is_list = scm_list_p (ret_val); */

  int length;
  length = scm_to_int(scm_length (ret_val));

  /* Start from 1 as the zero-th element only denotes query type */
  int i;
  for(i = 1; i < length; i++){
    SCM elm = scm_list_ref(ret_val, scm_from_int(i));
    char *anton = scm_to_locale_string (elm);
    printf("%s ", anton);
  }
  printf("\n");
}

/* Initialise Guile and load the Scheme file containing procedures */
void init(void){
  scm_init_guile();
  scm_c_primitive_load ("../words.scm");
}

int main(int argc, char *argv[]){
  int add_to_hist_flg = 0;
  int show_version_flg = 0;
  char *cvalue = NULL;

  char *action;

  int index;
  int c;

  opterr = 0;
  /* Get the action and the word from command line */

  while((c = getopt(argc, argv, "vhc:")) != -1){
    switch(c){
    case 'h':
      add_to_hist_flg = 1;
      break;
    case 'v':
      show_version_flg = 1;
      break;
    case 'c':
      cvalue = optarg;
      break;
    case '?':
      if(optopt == 'c'){
        fprintf(stderr,
                "Option -%c requires an argument.\n",
                optopt);
      }else if(isprint(optopt)){
        fprintf(stderr,
                "Unknown option `-%c'.\n",
                optopt);
      }else{
        fprintf(stderr,
                "Unknown option character `\\x%x'.\n",
                optopt);
      }
      return 1;
    default:
      print_usage();
      abort();
    }
  }

  if(show_version_flg){
    print_version();
    return EXIT_SUCCESS;
  }

  init();

  int i = 0;
  for (index = optind; index < argc; index++){
    if(i == 0){
      /* The argument specifies the action, like antonym, synonym */
      action = argv[index];
    }else{
      process_phrase(action, argv[index], add_to_hist_flg);
    }
    i++;
  }

  return EXIT_SUCCESS;
}
