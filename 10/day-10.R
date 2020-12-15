# Input Data: input.txt

library(tidyverse)

# Get input

input <- read_tsv('10/input.txt',
                  col_names = FALSE)

# Part 1

# Find a chain that uses all of your adapters to connect the charging outlet to 
# your device's built-in adapter and count the joltage differences between the 
# charging outlet, the adapters, and your device. 
# What is the number of 1-jolt differences multiplied by the number of 3-jolt differences?

# Find device's built-in joltage adapter
adapter <- max(input$X1) + 3

# Charging outlet has 0 jolts
outlet <- 0

# For each step in adapter, it can only be 1-3 jolts higher than the current adapter
# and I want the minimum adapter that fits 

adapters <- input %>%
  rename(jolts = X1) %>%
  mutate(diff_1 = FALSE,
         diff_3 = FALSE,
         used = FALSE) %>%
  arrange(jolts)

curr_adapter <- outlet

find_adapter<- function(curr_adapter, increment, adapters){
  
  found <- FALSE
  i <- 1
  
  while(!found & i <= nrow(adapters)){
    
    if(adapters$jolts[i] == curr_adapter + increment){
      
      adapters$used[i] = TRUE
      
      found = TRUE
      
      if(increment == 1){
        
        adapters$diff_1[i] = TRUE
        
      }
      
      if(increment == 3){
        
        adapters$diff_3[i] = TRUE
        
      }
      
    }# end if
    
    else{
      
      i <- i + 1
      
    }# end else
    
  }# end while
  
  new_curr_adapter <- adapters$jolts[i]
  
  output <- list(adapters, found, new_curr_adapter)
  
  return(output)
  
}

while(curr_adapter + 3 != adapter){
  
  # Check curr_adapter + 1
  
  output <- find_adapter(curr_adapter, 1, adapters)
  
  # Check curr_adapter + 2
  if(!output[[2]]){
    
    output <- find_adapter(curr_adapter, 2, adapters)
    
    # Check curr_adapter + 3
    if(!output[[2]]){
      
      output <- find_adapter(curr_adapter, 3, adapters)
      
    }
    
  }
  
  # Update variables
  
  adapters <- output[[1]]

  curr_adapter <- output[[3]]
  
}# end while

# number of 1-jolt differences 
total_diff_1 <- adapters %>%
  filter(diff_1 == TRUE) %>%
  nrow(.)

# number of 3-jolt differences
total_diff_3 <- adapters %>%
  filter(diff_3 == TRUE) %>%
  nrow(.)

# Multpily
total_diff_1 * (total_diff_3 + 1) # Add in the last difference to final adapter


# Part 2 
# What is the total number of distinct ways you can arrange the adapters to 
# connect the charging outlet to your device? (0 to 195)
