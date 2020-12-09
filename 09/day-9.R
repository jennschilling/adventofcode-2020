# Input Data: input.txt

library(tidyverse)

# Get input

input <- read_tsv('09/input.txt',
                  col_names = FALSE)

# Part 1 
# Find the first number in the list (after the preamble) which is not the sum of 
# two of the 25 numbers before it. What is the first number that does not have this property?

input_num <- input %>%
  rename(number = X1) 

for(s in 26:nrow(input_num)){

  # First index of 25 preceding numbers
  f <- s - 25
  
  # Last index of 25 preceding numbers
  l <- s - 1
  
  # Target number
  target <- input_num$number[s]
  
  # Initialize Found
  found <- FALSE
  
  # Find numbers that sum to target number
  for(i in f:(l-1)){
    
    for(j in (f+1):l){
      
      if(input_num$number[i] + input_num$number[j] == target){
        
        found <- TRUE
        
        break
        
      }
      
    }
    
    if(found){
      
      break
      
    }
    
  }
  
  # No sum found
  if(!found){
    
    print(target)
    break
    
  }

}


# Part 2
# The final step in breaking the XMAS encryption relies on the invalid number you just found: 
# you must find a contiguous set of at least two numbers in your list which sum to the invalid
# number from step 1. To find the encryption weakness, add together the smallest and largest 
# number in this contiguous range.
# What is the encryption weakness in your XMAS-encrypted list of numbers?

for(x in 1:nrow(input)){
  
  # Get set of numbers
  numbers <- input_num[x:nrow(input),]
  
  # Set row number
  numbers <- numbers %>%
    mutate(row_num = row_number())
  
  # Compute running sum
  numbers <- numbers %>%
    mutate(cum_sum = cumsum(number))
  
  # See if target is found
  numbers <- numbers %>%
    mutate(found = cum_sum == target)
  
  # Check found
  found_target <- numbers %>%
    filter(found == TRUE)
  
  # Found it
  if(nrow(found_target) != 0){
    
    # Get last row number
    final_row_num <- found_target$row_num[1]
    
    # Get data that added to target
    list_numbers <- numbers %>%
      filter(row_num <= final_row_num)
    
    # Find smallest number
    min_num <- min(list_numbers$number)
    
    # Find largest number
    max_num <- max(list_numbers$number)
    
    # Return sum
    print(min_num + max_num)
    
    break
    
  }
  
}
