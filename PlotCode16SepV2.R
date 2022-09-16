#Plot2code 

#Does the distribution of PVol depend on sGS?
#It seems that the distribution of PVol depends on the level SGS.Those in category 2 have the highest PVol mean
data3 %>% 
  filter(!is.na(sGS)) %>% 
  ggplot(data, mapping = aes(x=as.factor(sGS), y=PVol))+
  geom_boxplot(mapping = NULL, stat = "boxplot", position ="dodge2")

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

data3 %>% 
  mutate(TimeToRecurrence_days = log(TimeToRecurrence_days)) %>%
  aov(TimeToRecurrence_days~AdjRadTherapy, data = .) %>% 
  broom::tidy()
