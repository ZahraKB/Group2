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
