library(tidyverse)
library(here)

data<-read_delim (here("groupproject", "exam_nontidy.txt"))

#exploring data
#checking if any column starts with a number or has any spaces

glimpse(data)

#checking the first and last few columns
head(data)
tail(data)

#checking if any of the columns contained combined variables, which need to be in separate columns
View(data)

#summary of statistics 
skimr::skim(data)

#exploring the missing values
naniar::gg_miss_var(data)
  colSums(is.na(data))
#removing duplicates
data<-
data %>% 
  distinct()

#removing unnecessary columns (according to the exam description)
data<-
data %>% 
  select(-AA, -bGS, -`BN+`,-OrganConfined)

#renaming some columns that had long names, space in between characters, or started with a number
data<-
  data %>% 
  rename(volmx=`volume measurement`, 
         value=.value,
         age=`1_Age`,
         t_stage=`T stage`)

#Create new hospital variable that breaks up 'subject' into two variables Hospitalname and id.
data<-
data %>% 
  separate(col = subject, 
           into = c("Hospitalname", "id"), 
           sep = "-")
           
          
#creating two columns for PVol and TVol
data<- 
    data %>% 
  pivot_wider(names_from = volmx,
              values_from =value)


#creating a column that shows PVol values as high and low
data<-
data %>% 
  mutate(pvol_binary=if_else(PVol>100, "High", "Low"))

#creating a column which shows the time to recurrence as days
data<-
  data %>% 
  mutate(TimeToRecurrence_days=if_else(TimeToRecurrence_unit=="week", (TimeToRecurrence*7), TimeToRecurrence))

#then, removing the old time to recurrence columns as now they are unnecessary
 data<- 
data %>% 
  select(-TimeToRecurrence, -TimeToRecurrence_unit)
  

  #A column showing Recurrence as Yes/No
 data<-
    data %>% 
    mutate(Recurrence=if_else(Recurrence==0, "NO", "Yes"))
 
 #A numeric column showing multiplication of AnyAdjTherapy and PreopTherapy for each person
 
 data<- 
   data %>% 
   mutate(PreopAdjTherapy=AnyAdjTherapy*PreopTherapy) 
 
 
 #ordering columns 
 
 data<-
   data %>% 
   select(id, Hospitalname, age, everything())
 
 #arranging the IDs in increasing order
 data<-
   data %>% 
   arrange(id)

  
 #change id in orginal data to 'double'(id in the original data and the merge data need to be the same type)
 
  data<-  
    data %>%
      mutate(id = as.numeric(id))
  
  #Reading the second dataset and calling it data2
  data2<-
  read_delim(here("groupproject", "exam_joindata.txt"))
  

  #Join new data to original data
  
  data<- 
    data %>%
        full_join(data2, by = "id")
  view(data)
  #piping all the changes above
 
#exploring the data, checking the number of missing values in each columns
colSums(is.na(data))
naniar::gg_miss_var(data)
names(which(colSums(is.na(data))>0))

#Stratify your data by a categorical column and report min, max, mean and sd of a numeric column.

#Stratify by Recurrence (categorical) and summarizing age (numeric)
recurrence_age <- data %>%
    group_by(Recurrence) %>%
    summarise(max(age, na.rm = T),
      min(age, na.rm = T),
      mean(age, na.rm = T),
       sd(age, na.rm = T))
recurrence_age

#Stratify your data by a categorical column and report min, max, mean and sd of a numeric column for a defined set of observations - use pipe!

##Only for persons with T.Stage == 1

recurrence_t_stage<- data %>%
    filter(t_stage ==1) %>%
    group_by(Recurrence) %>%
    summarise(max(age, na.rm = T),
    min(age, na.rm = T),
   mean(age, na.rm = T),
    sd(age, na.rm = T))

recurrence_t_stage

#Only for persons with Median.RBC.Age == 25

Median.RBC_recurrence<- data %>%
  
  filter(Median.RBC.Age ==25) %>%
  group_by(Recurrence) %>%
  summarise(max(age, na.rm = T),
       min(age, na.rm = T),
      mean(age, na.rm = T),
       sd(age, na.rm = T))

Median.RBC_recurrence

# Only for persons with TimeToReccurence later than 4 days

day4_recurrence<-data %>%
    filter(TimeToRecurrence_days >4) %>%
    group_by(Recurrence) %>% 
    summarise(max(age, na.rm = T),
      min(age, na.rm = T),
     mean(age, na.rm = T),
      sd(age, na.rm = T))

day4_recurrence


#Only for persons recruited in Hosp1 and Tvol == 2

recurrence_hospital_TVol <- data %>%
  filter(Hospitalname == "Hosp1", TVol==2) %>%
  group_by(Recurrence) %>%
  summarise(max(age, na.rm = T),
        min(age, na.rm = T),
       mean(age, na.rm = T),
        sd(age, na.rm = T))
recurrence_hospital_TVol
#Use two categorical columns in your dataset to create a table (hint: ?count)

data %>%
   count(Recurrence,pvol_binary)


####Does the distribution of PreopPSA depend on T.Stage?
 #From the plot, it seems that the 
 
 data %>% 
   filter(!is.na(t_stage)) %>% 
   ggplot(data, mapping = aes(x=as.factor(t_stage), y=PreopPSA))+
   geom_boxplot(mapping = NULL, stat = "boxplot", position ="dodge2")####Does the distribution of PreopPSA depend on T.Stage?
 #From the plot, it seems that the 
 data<-
 data %>% 
   filter(!is.na(t_stage)) %>% 
   ggplot(data, mapping = aes(x=as.factor(t_stage), y=PreopPSA))+
   geom_boxplot(mapping = NULL, stat = "boxplot", position ="dodge2")
 
 
#Data analysis:
 #Was the time to recurrence different for various T.Stage levels?
 #Answer:yes, based on ANOVA and T.Test.
 data %>% 
   mutate(TimeToRecurrence_days = log(TimeToRecurrence_days)) %>%
   aov(TimeToRecurrence_days~t_stage, data = .) %>% 
   broom::tidy()
 
 data %>% 
   mutate(TimeToRecurrence_days = log(TimeToRecurrence_days)) %>%
   t.test(TimeToRecurrence_days~t_stage, data = .) %>% 
   broom::tidy() 
