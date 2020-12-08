# Input Data: input.txt

library(tidyverse)
library(stringr)

# Get input

input <- read_tsv('08/input.txt',
                  col_names = FALSE)

# Part 1 Immediately before any instruction is executed a second time, what value is in the accumulator?

# Initialize accumulator
acc <- 0

# Make instructions
instruc <- input %>%
  separate(X1, c('type', 'inc'), sep = " ") %>%
  mutate(inc = parse_number(inc),
         visited = FALSE,
         step = 0)

# Initialize index
i <- 1

# Initialize step
j <- 1
  
# Play the game
while(!instruc$visited[i]){
  
  instruc$visited[i] <- TRUE 
  instruc$step[i] <- j 
  j <- j + 1
  
  if(instruc$type[i] == 'acc'){
      
      acc <- acc + instruc$inc[i]
      i <- i + 1
      
  }
  
  else if(instruc$type[i] == 'nop'){
    
    i <- i + 1
    
  }
  
  else if(instruc$type[i] == 'jmp'){
    
    i <- i + instruc$inc[i]
    
  }
    
}

print(acc)


# Part 2
# The program is supposed to terminate by attempting to execute an instruction immediately 
# after the last instruction in the file.
# Fix the program so that it terminates normally by changing exactly one jmp (to nop) or nop (to jmp). 
# What is the value of the accumulator after the program terminates?
  
# Correct termination, i <- 633

# Change each jmp or nop and see if termination happens correctly

# Make instructions
instruc <- input %>%
  separate(X1, c('type', 'inc'), sep = " ") %>%
  mutate(inc = parse_number(inc),
         visited = FALSE,
         step = 0)

for(k in 1:nrow(instruc)){
  
  # Change types
  
  if(instruc$type[k] == "nop"){
    
    instruc$type[k] <- "jmp"
    
  }else if(instruc$type[k] == "jmp"){
    
    instruc$type[k] <- "nop"
  }
  
  # Initialize accumulator
  acc <- 0
  
  # Initialize index
  i <- 1
  
  # Initialize step
  j <- 1
  
  # Initialize visited and step
  instruc <- instruc %>%
    mutate(visited = FALSE,
           step = 0)
  
  # Play the game
  while(!instruc$visited[i] & !is.na(instruc$visited[i])){
    
    instruc$visited[i] <- TRUE 
    instruc$step[i] <- j 
    j <- j + 1
    
    if(instruc$type[i] == 'acc'){
      
      acc <- acc + instruc$inc[i]
      i <- i + 1
      
    }
    
    else if(instruc$type[i] == 'nop'){
      
      i <- i + 1
      
    }
    
    else if(instruc$type[i] == 'jmp'){
      
      i <- i + instruc$inc[i]
      
    }
    
    if(i > nrow(instruc)){
      
      break # successful termination
      
    }
    
  }
  
  # Failure - change the values back
  if(instruc$visited[i]  & !is.na(instruc$visited[i])){
    
    if(instruc$type[k] == "nop"){
      
      instruc$type[k] <- "jmp"
      
    }else if(instruc$type[k] == "jmp"){
      
      instruc$type[k] <- "nop"
    }
    
  }
  # Success
  else{
    
    print(acc)
    
    break
    
  }
  
}
