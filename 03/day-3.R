
# Input Data: input.txt

library(tidyverse)

input <- read_tsv('03/input.txt',
                  col_names = FALSE)

# Part 1
# Starting at the top-left corner of your map and following a slope of right 3 and down 1, 
# how many trees would you encounter?