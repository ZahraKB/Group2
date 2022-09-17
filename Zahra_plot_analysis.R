#Does the distribution of PreopPSA depend on t_Stage?

#Answer:t_stage is a set of discrete values, while PreopPSA has continuous values.
#I used geom_boxplot()to visualize the data
data<-
  data %>% 
  filter(!is.na(t_stage)) %>% 
  ggplot(data, mapping = aes(x=as.factor(t_stage), y=PreopPSA))+
  geom_boxplot(mapping = NULL, stat = "boxplot", position ="dodge2")

#based on the plot, it seems that higher t_stage is accompanied by higher distribution of PreopPSA values.

#Day 8: Analyse the dataset and answer the following question:
#Was the time to recurrence different for various t_Stage levels?
#Answer: Since t_stage has 2 categories, I used T.Test to assess the differences between the two categories in terms of the time to recurrence.
#Based on T.Test, there is a significant difference between various t_stage levels in terms of time to recurrence. 

data<-
  data %>% 
  mutate(TimeToRecurrence_days = log(TimeToRecurrence_days)) %>%
  t.test(TimeToRecurrence_days~t_stage, data = .) %>% 
  broom::tidy() 
