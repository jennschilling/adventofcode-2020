# Input Data: input.txt

library(tidyverse)

input <- read_tsv('01/input.txt',
                    col_names = FALSE)

# Part 1: Find the two entries that sum to 2020; what do you get if you multiply them together?

for(i in 1:199){
  
  num1 <- input[i,1]
  
  for(j in 2:200){
    
    num2 <- input[j,1]
    
    if(num1 + num2 == 2020){
      # FOUND IT
      print(num1)
      print(num2)
      print(num1 * num2)
      
      break
      
    }
    
  }
  
  if(num1 + num2 == 2020){
    
    break
    
  }
  
}


# Part 2: Find the three entries that sum to 2020; what do you get if you multiply them together?


input <- input %>% arrange(X1)

for(i in 1:198){
  
  num1 <- input[i,1]
  
  for(j in 2:199){
    
    num2 <- input[j,1]
    
    for(k in 3:200){
      
      num3 <- input[k,1]
      
      if(num1 + num2 + num3 == 2020){
        # FOUND IT
        print(num1)
        print(num2)
        print(num3)
        print(num1 * num2 * num3)
        
        break
        
      }
      
    }
    
    if(num1 + num2 + num3 == 2020){
      
      break
      
    }
    
  }
  
  if(num1 + num2 + num3 == 2020){
    
    break
    
  }
  
}
