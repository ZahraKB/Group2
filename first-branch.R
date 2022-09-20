# Hello TJ, welcome to your branch! :)
# Once again

###### DAY 8
### Task 4: Did those that had recurrence had also larger TVol values than those without recurrence?

# Here we see at TVol for those with (1) and without recurrence (0):

# First I need to load the required packages: 
library(tidyverse)
library(medicaldata)
library(ggpubr)
library(rstatix)

# Before performing any statistical test, I need to find out whether if the variables are normally distributed or not: 

df %>%
  group_by(Recurrence) %>%
  shapiro_test(TVol)

# If output p-value >0.05 -> can assume the normality. Then I can use parametric test (t-test): 
df %>% 
  mutate(TVol = log(TVol)) %>%
  group_by(Recurrence) %>%
  do(
    t.test(.$TVol, mu = 4) %>%
      broom::tidy()
  )

df %>% 
  mutate(TVol = log(TVol)) %>%
  filter(Recurrence==0) %>%
    t.test(.$TVol, mu = 4) %>%
      broom::tidy()

df %>% 
  mutate(TVol = log(TVol)) %>%
  filter(Recurrence==1) %>%
  t.test(.$TVol, mu = 4) %>%
  broom::tidy()

# If not-normally distributed (p<0.05), then use non-parametric test (Kruskalwallis):
df %>% 
  kruskal.test(TVol~Recurrence, data = .) %>%
  broom::tidy()

# If significant p-value (p<0.05) -> different TVol for those with recurrence vs without recurrence



###### DAY 7

### Task 2

ggplot(data = examData, aes(x = "PVol", y = "TVol")) + 
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)

cor_coefs <- cor.test(examData$PVol, examData$TVol)





