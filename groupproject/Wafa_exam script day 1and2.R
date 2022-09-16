library(tidyverse)
library(here)
data<- read_delim (here("groupproject", "exam_nontidy.txt"))
glimpse(data) # checking whether any column starts with a number or space 
# yes, the volume measurement variable has a space in the variable name 
skimr::skim(data)

# Are there any colmuns containing combined variables 
view(data)
# the second last column. i.e. volume measurement has two types pvol and tvol which shold be separate columns/variables 



  Make changes in variable types: 
    
    
     # renaming two variables
    data <- 
    data %>% 
    rename(ID=subject, 
           age=`1_Age`)

  # renaming variables with awkward names  
  data <- 
    data %>% 
    rename(volmx=`volume measurement`, 
           value=.value,
           bn=`BN+`,
           t_stage=`T stage`)
  
  # removing duplicates 
  
  data<-distinct(data)
  glimpse(data)
  view(data)
  
  #Are all variables as columns? 
  
  # divide volume measurement into two columns pvol and tvol  
  
  data<- data %>% 
    pivot_wider(names_from = volmx,
                values_from =value)
    
 ## create a set of new columns 
  
  data<-
    data %>% 
    mutate(pvol_binary=if_else(PVol>100, "High", "Low"))
  
  
  ttr<-
    data %>% 
    mutate(TimeToRecurrence_days=if_else(TimeToRecurrence_unit=="week", (TimeToRecurrence*7), TimeToRecurrence))

 
  

  
      
    
  
  
  
  
  
  
    
    
  
  
  

           
