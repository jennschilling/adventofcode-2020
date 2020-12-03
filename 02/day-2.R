# Input Data: input.txt

library(tidyverse)
library(stringr)

input <- read_tsv('02/input.txt',
                    col_names = FALSE)

# Part 1: How many passwords are valid according to their policies?

input_sep <- input %>%
  separate(X1, c('policy', 'pswd'), sep = ': ') %>%
  separate(policy, c('num', 'char'), sep = ' ') %>%
  separate(num, c('min', 'max'), sep = '-') %>%
  mutate(min = parse_number(min),
         max = parse_number(max)) %>%
  mutate(count_char = str_count(pswd, char)) %>%
  mutate(valid = min <= count_char & count_char <= max) 

input_sep %>% count(valid)

# Part 2: New policy, numbers are the positions of the letter and each pswd must have only one 

input_sep_2 <- input %>%
  separate(X1, c('policy', 'pswd'), sep = ': ') %>%
  separate(policy, c('num', 'char'), sep = ' ') %>%
  separate(num, c('pos1', 'pos2'), sep = '-') %>%
  mutate(pos1 = parse_number(pos1),
         pos2 = parse_number(pos2)) %>%
  mutate(char_pos1 = substr(pswd, pos1, pos1),
         char_pos2 = substr(pswd, pos2, pos2)) %>%
  mutate(one_match = (char_pos1 == char & char_pos2 != char) | 
                     (char_pos1 != char & char_pos2 == char))

input_sep_2 %>% count(one_match)
