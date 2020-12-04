
# Input Data: input.txt

library(tidyverse)
library(stringr)

input <- read_file('04/input.txt')

# Part 1
# Count the number of valid passports - those that have all required fields. 
# Treat cid as optional. In your batch file, how many passports are valid?

# Required Fields
# byr, iyr, eyr, hgt, hcl, ecl, pid, cid (optional)

# \r\n\r\n is the pattern between passports


input_dat <- str_split(input, '\r\n\r\n')

input_df <- data.frame(unlist(input_dat)) %>%
  rename(passport = unlist.input_dat.) %>%
  mutate(byr = str_detect(passport, 'byr'),
         iyr = str_detect(passport, 'iyr'),
         eyr = str_detect(passport, 'eyr'),
         hgt = str_detect(passport, 'hgt'),
         hcl = str_detect(passport, 'hcl'),
         ecl = str_detect(passport, 'ecl'),
         pid = str_detect(passport, 'pid'),
         valid = byr & iyr & eyr & hgt & hcl & ecl & pid)

table(input_df$valid)

# Part 2

# Data must follow rules below and required fields must be present
# Rules
# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
#   If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.

input_df_2 <- data.frame(unlist(input_dat)) %>%
  rename(passport = unlist.input_dat.) %>%
  mutate(passport = str_replace_all(passport, '\r\n', ' ')) %>%
  mutate(byr = str_detect(passport, 'byr'),
         iyr = str_detect(passport, 'iyr'),
         eyr = str_detect(passport, 'eyr'),
         hgt = str_detect(passport, 'hgt'),
         hcl = str_detect(passport, 'hcl'),
         ecl = str_detect(passport, 'ecl'),
         pid = str_detect(passport, 'pid'),
         valid = byr & iyr & eyr & hgt & hcl & ecl & pid) %>%
  filter(valid == TRUE) %>%
  separate(passport, 
           c('col1', 'col2', 'col3', 'col4', 
             'col5', 'col6', 'col7', 'col8'),
           sep = ' ') %>%
  mutate(rownum = row_number()) %>%
  select(-byr, -iyr, -eyr, -hgt, -hcl, -ecl, -pid, -valid) %>%
  pivot_longer(col1:col8, names_to = "col", values_to = "val") %>%
  select(-col) %>%
  separate(val, c('id', 'value'), sep = ':') %>%
  filter(!is.na(value)) %>%
  filter(id != 'cid') %>%
  mutate(check = (id == 'byr' & value >= 1920 & value <= 2002) |
           (id == 'iyr' & value >= 2010 & value <= 2020) |
           (id == 'eyr' & value >= 2020 & value <= 2030) |
           ((id == 'hgt' & (str_detect(value, "cm") &  
               parse_number(value) >= 150 & parse_number(value) <= 193)) | 
              (id == 'hgt' & (str_detect(value, "in") &  
                 parse_number(value) >= 59 & parse_number(value) <= 76))) |
           (id == 'hcl' & str_detect(value, '#[0-9a-fA-F]{6}')) |
           (id == 'ecl' & (value == 'amb' | value == 'blu' | value == 'brn' |
                           value == 'gry' | value == 'grn' | value ==  'hzl' |
                           value == 'oth')) |
           (id == 'pid' & nchar(value) == 9)
         ) %>%
  pivot_wider(rownum, names_from = id, values_from = check) %>%
  mutate(valid = byr & iyr & eyr & hgt & hcl & ecl & pid)

table(input_df_2$valid)
