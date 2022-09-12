#load packages
library(tidyverse)
library(here)

#Reading data
read_delim (here("groupproject", "exam_nontidy.txt" ))

data <- read_delim (here("groupproject", "exam_nontidy.txt" ))


##Are all variables as columns ?
View(data)
glimpse(data)
View(data)
head(data)
## No, not all variables are columns.The last varibale .value should be split into pvol and tvol with values assigned from the value column 
##There are 21 columns and 672 rows. 
