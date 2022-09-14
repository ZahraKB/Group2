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

data <- data %>% 
  pivot_wider(names_from = volmx,
              values_from =value) 

view(data) 

data<-
  data %>% 
  mutate (pvol_binary = if_else (PVol >100, "High", "Low")) 


#A numeric column showing TimeToReccurence in days (or weeks if you like) is missing


#A column showing recurrence as Yes/No
data<-
  data %>% 
  mutate(Recurrence=if_else(Recurrence==0, "NO", "Yes")) 

#A numeric column showing multiplication of AnyAdjTherapy and PreopTherapy for each person

data<- 
  data %>% 
  mutate(PreopAdjTherapy=AnyAdjTherapy*PreopTherapy) 

#Create new hospital variable that breaks up 'subject'/Hospital into two variables and extracts the ID. 
data
data<-data %>% 
  separate(col = ID, 
           into = c("Hospitalname", "id"), 
           sep = "-")
data

#ordering the columns

data<-
  data %>% 
  select(id, Hospitalname, age, everything()) 

#arranging the IDs in increasing order
data<-
  data %>% 
  arrange(id) 

data

##Afterclass code----
#Read new dataset - exam_joindata.txt

data2 <-read_delim (here("groupproject", "exam_joindata.txt"))
data2
data

#creating a numeric variable that converts weeks in timetorecurrence to days
data<- data %>%
  mutate(TimeToRecurrence_days=if_else(TimeToRecurrence_unit=="week", (TimeToRecurrence*7), TimeToRecurrence))
view(data)

#change id in orginal data to 'double'(id in the original data and the merge data need to be the same type)

 data<-  data %>%
mutate(id = as.numeric(id)) 
  
#Join new data to original data
data3<- data %>%
  full_join(data2, by = "id")

#Check for missing values
#Count the number of missing values per column
colSums(is.na(data3)) 
#Names the number of columns with missing data. 
names(which(colSums(is.na(data3))>0))
#All the following variables have missing data. 


#Stratify your data by a categorical column and report min, max, mean and sd of a numeric column.
#Stratify by Recurrence (categorical) and summarizing age (numeric)

data <- data %>% 
  group_by(Recurrence) %>% 
  summarise(max(age, na.rm = T), 
            min(age, na.rm = T), 
            mean(age, na.rm = T), 
            sd(age, na.rm = T))
  

#Stratify your data by a categorical column and report min, max, mean and sd of a numeric column for a defined set of observations - use pipe!
##Only for persons with T.Stage == 1
data<- data %>% 
  filter(t_stage ==1) %>% 
  group_by(Recurrence) %>% 
  summarise(max(age, na.rm = T),
            min(age, na.rm = T),
            mean(age, na.rm = T), 
            sd(age, na.rm = T))

#Only for persons with Median.RBC.Age == 25
data<- data %>% 
  filter(Median.RBC.Age ==25) %>% 
  group_by(Recurrence) %>% 
  summarise(max(age, na.rm = T),
            min(age, na.rm = T),
            mean(age, na.rm = T), 
            sd(age, na.rm = T))

# Only for persons with TimeToReccurence later than 4 weeks
#Did not run code below.
view(data)
data<-data %>% 
  filter(TimeToRecurrence <4) %>% 
  group_by(Recurrence) %>% 
  summarise(max(age, na.rm = T),
            min(age, na.rm = T),
            mean(age, na.rm = T), 
            sd(age, na.rm = T)) #Need to change timetorecurrence to weeks

#Only for persons recruited in Hosp1 and Tvol == 2
 data <- data %>% 
  filter(Hospitalname == "Hosp1", TVol==2) %>% 
  group_by(Recurrence) %>% 
  summarise(max(age, na.rm = T),
            min(age, na.rm = T),
            mean(age, na.rm = T), 
            sd(age, na.rm = T)) 

#Use two categorical columns in your dataset to create a table (hint: ?count)
 data3 %>% 
   count(Recurrence,pvol_binary)
 #table(data3$Recurrence, data3$pvol_binary)
 
