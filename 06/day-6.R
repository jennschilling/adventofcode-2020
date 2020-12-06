# Input Data: input.txt

library(tidyverse)
library(stringr)

# Get input

input <- read_file('06/input.txt')

# Process input

input_dat <- str_split(input, '\r\n\r\n')

input_df <- data.frame(unlist(input_dat)) %>%
  rename(customs = unlist.input_dat.)

# Part 1 
# For each group, count the number of questions to which anyone answered "yes".
# What is the sum of those counts?

# If a letter is present for the group/row, then a person answered "yes"

output <- input_df %>%
  mutate(count_yes = 0)

# Count number of unique characters per row
for(i in 1:nrow(output)){
  
  output$count_yes[i] <- sum(!!str_count(output$customs[i], letters))
  
}

# Sum counts
sum(output$count_yes)

# Part 2
# For each group, count the number of questions to which everyone answered "yes"
# What is the sum of those counts?

output <- output %>%
  mutate(all_yes = 0)

# Count number of characters that appear for everyone
for(i in 1:nrow(output)){
  
  # Get group data
  group <- output$customs[i]
  
  # Get data for individuals in group
  individuals <- str_split(group, "\\r\\n")
  num_individuals <- length(individuals[[1]])
  
  # Get list of characters in customs list
  
  char_list <- individuals[[1]][1]
  
  for(j in 2:num_individuals){
    
    new_list <- str_split(individuals[[1]][j], "")
    
    for(k in 1:length(new_list[[1]])){
      
      if(!is.na(new_list[[1]][k])){
      
        if(!str_detect(char_list, new_list[[1]][k])){
        
          char_list <- paste0(char_list, new_list[[1]][k])
        
        } # end if
        
      } # end if
      
    } # end for k
    
  } # end  for j
  
  # Count the number of characters that appear for everyone
  
  num_match <- 0
  
  for(m in 1:str_length(char_list)){
    
    target_char <- substr(char_list, m, m)
    
    count_yes <- 0
    
    for(n in 1:num_individuals){
      
      if(str_detect(individuals[[1]][n], target_char)){
        
        count_yes <- count_yes + 1
        
      } # end if
      
    } # end  for n
    
    if(count_yes == num_individuals){
      
      num_match <- num_match + 1
      
    } # end if
    
  } # end for m
  
  # Assign count to data frame
  output$all_yes[i] <- num_match 
  
} # end for i

# Sum counts
sum(output$all_yes)
