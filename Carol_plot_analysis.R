

#Does the distribution of PVol depend on sGS?
data %>% 
  ggplot(aes(x = PVol, y = sGS)) +
  geom_point()


#Did having AdjRadTherapy affected time to recurrence?

#Boxplot showing AdjRadtheory vs recurrence
Anyadjtherapy_recurrence <-data %>%
  filter(!is.na(TimeToRecurrence_days)) %>%
  ggplot(data, mapping = aes(x=as.factor(AdjRadTherapy), y=TimeToRecurrence_days))+
  geom_boxplot(mapping = NULL, stat = "boxplot", position ="dodge2")

Anyadjtherapy_recurrence 