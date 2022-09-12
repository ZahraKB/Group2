library(tidyverse)
library(here)
read_delim (here("groupproject", "exam_nontidy.txt"))
glimpse(data) # checking whether any column starts with a number or space 


# Are there any colmuns containing combined variables 
view(data)
# the second last column i.e. volume measurement has two types pvol and tvol which shold be separate columns/variables 




           