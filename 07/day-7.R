# Input Data: input.txt

library(tidyverse)
library(stringr)

# Get input

input <- read_tsv('07/input.txt',
                  col_names = FALSE)

# Part 1 How many bag colors can eventually contain at least one shiny gold bag?

# Find "shiny gold" in the bag rules 
rules <- input %>%
  rename(bag_rule = X1) %>%
  mutate(shiny_gold = str_detect(bag_rule, "shiny gold bag"))

# Pull out matched bags
shiny_gold <- rules %>% filter(shiny_gold == TRUE)

# Update to remaining bags
rules <- rules %>% filter(shiny_gold == FALSE)

# Now get bags that contain bags that have "shiny gold" in them
check_bags <- function(rules, shiny_gold){

  # Get the bags that contain "shiny gold" and remove the plural
  bags <- shiny_gold %>%
    mutate(outer_bag = word(bag_rule, 1, 3, sep = " "),
           outer_bag = substr(outer_bag, 1, str_length(outer_bag) - 1))
  
  # Find bags that contain bags with "shiny gold"
  for(i in 1:nrow(bags)){
    
    bag <- bags$outer_bag[i]
    
    rules <- rules %>%
      mutate(shiny_gold = ifelse(shiny_gold, shiny_gold,
                                 str_detect(bag_rule, bag)))
    
  }

  return(rules)
  
}

n <- 0

all_shiny_gold <- shiny_gold

# Keep finding "shiny gold" bags within bags within bags 
while(n != nrow(all_shiny_gold)){
  
  # Number of "shiny gold" bags within
  n <- nrow(all_shiny_gold)
  
  # Check for next round of "shiny gold"
  rules <- check_bags(rules, shiny_gold)
  
  # Pull out matched bags
  shiny_gold <- rules %>% filter(shiny_gold == TRUE)
  
  all_shiny_gold <- rbind(all_shiny_gold, shiny_gold)
  
  # Update to remaining bags
  rules <- rules %>% filter(shiny_gold == FALSE | is.na(shiny_gold))
  
}

# Get the final number of bags within bags within bags of "shiny gold"
nrow(all_shiny_gold) - 1 # subtract the rule for "shiny gold bags contain.."

# Part 2 - How many individual bags are required inside your single shiny gold bag?

# Iteratively get the number of bags required


find_req_bags <- function(num_bags, req_bags){
  
  # Remove "contains" first portion of string and period
  req_bags <- req_bags %>%
    mutate(bag_rule = str_remove(bag_rule, ".*contain ")) %>%
    mutate(bag_rule = str_remove(bag_rule, "\\."))
  
  # Get list of bags inside the bags
  bag_list <- unlist(str_split(req_bags$bag_rule, ", "))
  
  new_req_bags <- data.frame()
  
  # Compute number of bags inside the required bags
  # Get the next set of required bags
  for(i in 1:length(bag_list)){
    
    if(bag_list[i] != "no other bags"){
    
       # Increment number of bags ### PROBLEM - cannot just add number of bags, it's multiplicative
       num_bags <- num_bags + parse_number(bag_list[i])
       
       # Get bag details
       outer_bag <- word(bag_list[i], 2, 4, sep = " ")
       
       if(substr(outer_bag, str_length(outer_bag), str_length(outer_bag)) != 's'){
         
         outer_bag <- paste0(outer_bag, 's')
         
       }
       
       outer_bag <- paste0("^", outer_bag)
       
       # Add to list of bag requirements
       new_req_bags <- rbind(new_req_bags,
                             rules %>% filter(str_detect(bag_rule, outer_bag)))
       
      }
    
    }
    
  result <- list(num_bags, new_req_bags)
  
  return(result)
  
}


# Initialize Variables 
num_bags <- 0

rules <- input %>%
  rename(bag_rule = X1) 

req_bags <- rules %>%
  filter(str_detect(bag_rule, "^shiny gold bag"))

# Search for bags in bags
while(!is_empty(req_bags)){
  
  n <- find_req_bags(num_bags, req_bags)
  
  num_bags <- n[[1]]
  
  req_bags <- n[[2]]
  
  
}


