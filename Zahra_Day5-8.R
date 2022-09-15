library(tidyverse)
library(here)
read_delim(here("groupproject", "exam_nontidy.txt"))
data<-read_delim (here("groupproject", "exam_nontidy.txt"))

data <- 
  data %>% 
  rename(ID=subject, 
         age=`1_Age`)

data <- 
  data %>% 
  rename(volmx=`volume measurement`, 
         value=.value,
         bn=`BN+`,
         t_stage=`T stage`)

data<-distinct(data)

 data<-
data %>% 
  pivot_wider(names_from = volmx,
              values_from =value)
 
 view(data)

 data<-
   data %>% 
   mutate(pvol_binary=if_else(PVol>100, "High", "Low"))

 view(data) 

 #A numeric column showing TimeToReccurence in days (or weeks if you like) 
 
 data<-
 data %>% 
   mutate(TimeToRecurrence_days=if_else(TimeToRecurrence_unit=="week", (TimeToRecurrence*7), TimeToRecurrence))
 View(data)
 
 data<-
   data %>% 
   select(-TimeToRecurrence, -TimeToRecurrence_unit)
 #A column showing recurrence as Yes/No
 data<-
 data %>% 
   mutate(Recurrence=if_else(Recurrence==0, "NO", "Yes"))
   
 #A numeric column showing multiplication of AnyAdjTherapy and PreopTherapy for each person

 data<- 
data %>% 
  mutate(PreopAdjTherapy=AnyAdjTherapy*PreopTherapy)

 
 #creating unique ids in a separate column and hospital in another one
 
 data<-
  data %>% 
    rename(Hospital=ID)

 data<-
 data %>% 
   mutate(ID=1:n())

 #ordering the columns
 
 data<-
   data %>% 
   select(ID, Hospital, age, everything())

 #arranging the IDs in increasing order
 data<-
 data %>% 
   arrange(ID)
 
 

