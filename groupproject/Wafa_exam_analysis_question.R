
##Was the time to recurrence different for various RBC.Age.Group levels?


TimetoRecurrencebyRBC_age_group<- data %>% 
  mutate(TimeToRecurrence_days=log(TimeToRecurrence_days)) %>% 
  aov(TimeToRecurrence_days~RBC.Age.Group, data=.)
TimetoRecurrencebyRBC_age_group
summary(TimetoRecurrencebyRBC_age_group)
broom::tidy(TimetoRecurrencebyRBC_age_group)


## Answer to the question: since the p value for Anova is higher than 
## 0.05 it is not possible to reject the null hypothesis and
## and we cannot claim that the time to recurrence varied by RBC age group  
