
# Input Data: input.txt

library(tidyverse)

# Get input

input <- read_tsv('05/input.txt',
                  col_names = FALSE)

# Part 1

# What is the highest seat ID on a boarding pass?

# The first 7 characters will either be F or B; these specify exactly one of 
# the 128 rows on the plane (numbered 0 through 127). Each letter tells you 
# which half of a region the given seat is in. Start with the whole list of rows; 
# the first letter indicates whether the seat is in the front (0 through 63) or 
# the back (64 through 127). The next letter indicates which half of that region 
# the seat is in, and so on until you're left with exactly one row.

# The last three characters will be either L or R; these specify exactly one of
# the 8 columns of seats on the plane (numbered 0 through 7). The same process
# as above proceeds again, this time with only three steps. L means to keep the 
# lower half, while R means to keep the upper half.

# Every seat also has a unique seat ID: multiply the row by 8, then add the column. 

input_calc <- input %>%
  mutate(row = 0,
         col = 0,
         seat_id = 0) %>%
  rename(pass = X1)

for(i in 1:nrow(input)){
  
  # Initial Values
  min_row <- 0
  max_row <- 127
  
  min_col <- 0
  max_col <- 7
  
  # Boarding Pass
  pass <- input_calc[i, 1]$pass
  
  # Determine Row
  for(j in 1:7){
    
    pass_val <- substr(pass, j, j)
    
    if(pass_val == "F"){
      
      max_row = floor(max_row - ((max_row - min_row) / 2))
      
    }else{
      
      min_row = ceiling(min_row + ((max_row - min_row) / 2))
      
    }
    
    input_calc[i, 2] = min_row
    
  }
  
  # Determine Column
  for(k in 8:10){
    
    pass_val <- substr(pass, k, k)
    
    if(pass_val == "L"){
      
      max_col = floor(max_col - ((max_col - min_col) / 2))
      
    }else{
      
      min_col = ceiling(min_col + ((max_col - min_col) / 2))
      
    }
    
    input_calc[i, 3] = min_col
    
  }
  
}

# Compute Seat ID
input_calc <- input_calc %>%
  mutate(seat_id = row * 8 + col)

print(max(input_calc$seat_id)) # Answer

# Part 2

# It's a completely full flight, so your seat should be the only missing 
# boarding pass in your list. However, there's a catch: some of the seats 
# at the very front and back of the plane don't exist on this aircraft, so 
# they'll be missing from your list as well.

# Your seat wasn't at the very front or back, though; the seats with IDs +1
# and -1 from yours will be in your list.

# What is the ID of your seat?

# Search through the list and find the ID that is missing

input_calc_2 <- input_calc %>% 
  arrange(seat_id) %>%
  mutate(lag = lag(seat_id),
         check = lag + 1 == seat_id) %>%
  filter(check == FALSE)

print(input_calc_2$seat_id - 1) # Answer
  
