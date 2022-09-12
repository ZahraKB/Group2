library(tidyverse)
library(here)
read_delim (here("groupproject", "exam_nontidy.txt"))
glimpse(data) # checking whether any column starts with a number or space 
# yes, the volume measurement variable has a space in the variable name 


# Are there any colmuns containing combined variables 
view(data)
# the second last column i.e. volume measurement has two types pvol and tvol which shold be separate columns/variables 




           
