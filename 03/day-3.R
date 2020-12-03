
# Input Data: input.txt

library(tidyverse)

input <- read_tsv('03/input.txt',
                  col_names = FALSE)

# Part 1
# Starting at the top-left corner of your map and following a slope of right 3 and down 1, 
# how many trees would you encounter?

# Start at position 1, 1 - right 3, down 1
# Next position is 2, 4 - right 3, down 1
# Next position is 3, 7

ntrees <- 0

nrows <- nrow(input)

nchars <- nchar(input[1, 1])

pos <- 1

i <- 1

right <- 1

down <- 2

while(i <= nrows){
  
  square <- substr(input[i, 1], pos, pos)
  
  if(square == "#"){
    
    ntrees <- ntrees + 1
    
  }

  
  if(pos + right > nchars){
    
    pos <- pos + right - nchars
    
  }else{
    
    pos <- pos + right
    
  }

  i <- i + down
  
}

# Part 2
# Number of trees for the following patterns
# Right 1 Down 1 - 77
# Right 3 Down 1 - 218
# Right 5 Down 1 - 65
# Right 7 Down 1 - 82
# Right 1 Down 2- 43

# Get answer by multiplying together

77 * 218 * 65 * 82 * 43
