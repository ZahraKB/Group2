#Group2.Exam Data

#Author:Zahra
###

library(tidyverse)
library(here)
read_delim(here("groupproject", "exam_nontidy.txt"))
data<-read_delim (here("groupproject", "exam_nontidy.txt"))

#checking duplicates

data %>% 
  distinct()

#
#Check variable types comment on any changes you would like to make
#But do not make any changes 
glimpse(data)

#There are three variables in character
#but the rest are in db1 form (double class)
#at this point, we do not make any changes

