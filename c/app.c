/*
  Copyright (C) 2016 Sergo Pasoevi.

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this program.  If not, see
  <http://www.gnu.org/licenses/>.
*/

#include <stdio.h>
#include <stdbool.h>
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <libguile.h>

char *get_homedir(void)
{
    char homedir[MAX_PATH];
#ifdef _WIN32
    snprintf(homedir, MAX_PATH, "%s%s", getenv("HOMEDRIVE"), getenv("HOMEPATH"));
#else
    snprintf(homedir, MAX_PATH, "%s", getenv("HOME"));
#endif
    return strdup(homedir);
}

char *program_name = "dict";
char *version = "0.01b";
char *hist_fname = strcat(get_homedir(), "/.weehist");

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

void print_scheme_list(SCM lst){
  /* Calculate the size of the list returned from Scheme */
  int i, length;
  length = scm_to_int(scm_length (lst));
  /* Start from 1 as the zero-th element only denotes query type */
  for(i = 1; i < length; i++){
    SCM elm = scm_list_ref(lst, scm_from_int(i));
    char *anton = scm_to_locale_string (elm);
    printf("%s ", anton);
  }
  printf("\n");
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

void print_history(char *history_file){
  FILE * fp;
  char * line = NULL;
  size_t len = 0;
  ssize_t read;

  fp = fopen(history_file, "r");
  if (fp == NULL)
    exit(EXIT_FAILURE);

  while ((read = getline(&line, &len, fp)) != -1) {
    printf("%s", line);
  }

  fclose(fp);
  if (line)
    free(line);
}

void process_phrase(char *action,  char *word, bool add_to_hist){
  /* Add to history */
  if(add_to_hist){
    int errnum;
    errnum = add_to_history(word, hist_fname);
  }

  /* Look up and call the function */
  func_symbol = scm_c_lookup(action);
  func = scm_variable_ref(func_symbol);
  ret_val = scm_call_1(func, scm_from_locale_string(word));

  /* TODO: process output based on the result of the Scheme function */
  /* SCM is_list = scm_list_p (ret_val); */

  /* Print the output */
  print_scheme_list(ret_val);
}

/* Initialise Guile and load the Scheme file containing procedures */
void init(void){
  scm_init_guile();
  scm_c_primitive_load ("../scm/words.scm");
}

int main(int argc, char *argv[]){
  bool add_to_hist_flg = false;
  bool show_version_flg = false;
  bool print_hist_flg = false;
  char *action = NULL;

  int index;
  int c;

  opterr = 0;
  /* Get the action and the word from command line */

  while((c = getopt(argc, argv, "vhr")) != -1){
    switch(c){
    case 'h':
      add_to_hist_flg = true;
      break;
    case 'v':
      show_version_flg = true;
      break;
    case 'r':
      print_hist_flg = true;
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

  if(print_hist_flg){
    print_history(hist_fname);
  }

  init();

  /*
    Process the positional arguments: the first argument being the
    action and the remaining ones the words to look up.
  */
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
