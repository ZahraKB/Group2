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


#Plots and data analysis

# plot_Zahra

#Question: Does the distribution of PreopPSA depend on t_Stage?
#Answer:Answer:t_stage is a set of discrete values, while PreopPSA has continuous values. therefore, geom_boxplot()was used to visualize the data.
#Based on the plot, it seems that higher t_stage is accompanied by higher distribution of PreopPSA values.

plot1<-
 data %>% 
   filter(., !is.na(t_stage)) %>% 
   ggplot(data, mapping = aes(x=as.factor(t_stage), y=PreopPSA))+
   geom_boxplot(mapping = NULL, stat = "boxplot", position ="dodge2")
plot1


# Plot_Wafa
#Deos the distribution of TVol depend on sGS ?

data %>% 
  filter(!is.na(TVol), !is.na(sGS)) %>% 
  ggplot(data, mapping = aes(x=as.factor(sGS), y=(TVol)))+
  geom_count(mapping=NULL, data=NULL, stat = "sum", position="identity", na.rm=TRUE)


#Answer: most of the values of TVol are in sGS at level three and much more so than at sGS values of 1 and 4. So there seems to be some correlation between different values of sGs and TVol.

# Plot_Carol
#Does the distribution of PVol depend on sGS?*

#It seems that the distribution of PVol depends on the level SGS.Those in category 2 have the highest PVol mean*

 
data %>%

  filter(!is.na(sGS)) %>%

  ggplot(data, mapping = aes(x=as.factor(sGS), y=PVol))+

  geom_boxplot(mapping = NULL, stat = "boxplot", position ="dodge2")



# Plot Taraneh
# Is there a relation between the PVol and TVol variables?

library(hrbrthemes)
# Scatter plot with linear trend
ggplot(data, aes(x=PVol, y=TVol)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_ipsum()

# Correlation test

cor_coefs <- cor.test(data$PVol, data$TVol)
cor_coefs
# p-value=0.0002217 < 0.05 --> we can conclude that PVol and TVol are significantly correlated with correlation coefficient = -0.21


# Day 8 Analysis 

#Analysis_Zahra 

#Zahra t-test 

#Question: Was the time to recurrence different for various t_Stage levels?
 
  
 T_test<-
 data %>% 
  mutate(TimeToRecurrence_days = log(TimeToRecurrence_days)) %>%
  t.test(TimeToRecurrence_days~t_stage, data = .) %>% 
  broom::tidy() 
T_test
```
#Answer: Since t_stage has 2 categories, I used T.Test to assess the differences between the two categories in terms of the time to recurrence.
#Based on T.Test, there is a significant difference between various t_stage levels in terms of time to recurrence (p=0.0115 which is < 0.05).


# Analysis_Wafa

#Was the time to recurrence different for various RBC.Age.Group levels?

TimetoRecurrencebyRBC_age_group<- data %>% 
  mutate(TimeToRecurrence_days=log(TimeToRecurrence_days)) %>% 
  aov(TimeToRecurrence_days~RBC.Age.Group, data=.)
TimetoRecurrencebyRBC_age_group
summary(TimetoRecurrencebyRBC_age_group)
broom::tidy(TimetoRecurrencebyRBC_age_group)

# Answer to the question: since the p value for Anova is higher than 0.05 it is not possible to reject the null hypothesis 
#and we cannot claim that the time to recurrence varied by RBC age group.

# Analysis_Carol
#Did having AdjRadTherapy affected time to recurrence?
#Boxplot showing AdjRadtheory vs recurrence
#Results: This relationship is unclear because the there are very few 1's in the anyadjtherapy recurrence.
#However the mean is higher in those with 0's
#The p value value is 0.592.

Anyadjtherapy_recurrence <-data %>%

  filter(!is.na(TimeToRecurrence_days)) %>%

  ggplot(data, mapping = aes(x=as.factor(AdjRadTherapy), y=TimeToRecurrence_days))+

  geom_boxplot(mapping = NULL, stat = "boxplot", position ="dodge2")

Anyadjtherapy_recurrence

 
 data %>%

  mutate(TimeToRecurrence_days = log(TimeToRecurrence_days)) %>%

  aov(TimeToRecurrence_days~AdjRadTherapy, data = .) %>%

  broom::tidy()

# Analysis_Taraneh
Did those that had recurrence had also larger TVol values than those without recurrence?

# Here we see at TVol for those with (1) and without recurrence (0):

# First we need to load the required packages:
library(tidyverse)
library(medicaldata)
library(ggpubr)
library(rstatix)

# Before performing any statistical test, we need to find out whether if the variables are normally distributed or not:

data %>%
  group_by(Recurrence) %>%
  shapiro_test(TVol)

# Output p-value << 0.05 --> We cannot assume the normality, so we need to use non-parametric test (Kruskalwallis):

data %>%
  kruskal.test(TVol~Recurrence, data = .) %>%
  broom::tidy()

# Output p-value = 0.000000367 << 0.05, i.e. we have a significant p-value. It is possible to reject the null hypothesis. Thus, we can claim that the TVol is different for those with recurrence vs without recurrence.


