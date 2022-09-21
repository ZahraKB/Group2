

#Does the distribution of PVol depend on sGS?
data3 %>% 
  ggplot(aes(x = PVol, y = sGS, !is.na())) +
  geom_point()

view(data3)

#Did having AdjRadTherapy affected time to recurrence?

#Boxplot showing AdjRadtheory vs recurrence
Anyadjtherapy_recurrence <-data %>%
  filter(!is.na(TimeToRecurrence_days)) %>%
  ggplot(data, mapping = aes(x=as.factor(AdjRadTherapy), y=TimeToRecurrence_days))+
  geom_boxplot(mapping = NULL, stat = "boxplot", position ="dodge2")

Anyadjtherapy_recurrence 