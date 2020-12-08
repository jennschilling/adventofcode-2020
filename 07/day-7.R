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



