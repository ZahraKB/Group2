
### Does the distribution of TVol depend on sGS ?

data %>% 
  filter(!is.na(TVol), !is.na(sGS)) %>% 
  ggplot(data, mapping = aes(x=as.factor(sGS), y=(TVol)))+
  geom_count(mapping=NULL, data=NULL, stat = "sum", position="identity", na.rm=TRUE)

## Answer: most of the values of TVol are in sGS at level three and much 
## more so than at sGS values of 1 and 4. So there seems to be some correlation 
## between different values of sGs and TVol. 










###---- 
ggplot(data = data3, aes(x ="t_stage" , y = "PreopPSA")) + 
  geom_point() +
  geom_smooth(method=lm) 


 
data3 %>% 
  ggplot(data=data3, aes(x="TVol", y="sGS"))+
  geom_point()+
  geom_smooth(method="lm")
  

Day8: 
  
  
  Was the time to recurrence different for various RBC.Age.Group levels?
  
  
  TimetoRecurrencebyRBC_age_group<- data3 %>% 
  mutate(PVol=log(PVol)) %>% 
  aov(PVol~RBC.Age.Group, data=.) # data = . saying use the data above, imp bec we did log transform on a variable 
  summary()
  broom::tidy()  

  TimetoRecurrencebyRBC_age_group<- data3 %>% 
    mutate(TimeToRecurrence_days=log(TimeToRecurrence_days)) %>% 
    aov(TimeToRecurrence_days~RBC.Age.Group, data=.)
  TimetoRecurrencebyRBC_age_group
  summary(TimetoRecurrencebyRBC_age_group)
  broom::tidy(TimetoRecurrencebyRBC_age_group)
  
  TimetoRecurrencebyRBC_age_group %>% 
    broom::tidy() %>% 
    
  
  ## Answer to the question: since the p value for Anova is higher than 
    ## 0.05 it is not possible to reject the null hypothesis and
  ## and we cannot claim that the time to recurrence varied by RBC age group  
  
  
  ##Results of ANOVA 
  Call:
    aov(formula = TimeToRecurrence_days ~ RBC.Age.Group, data = .)
  
  Terms:
    RBC.Age.Group Residuals
  Sum of Squares         0.1835  590.9373
  Deg. of Freedom             1       313
  
  Residual standard error: 1.374037
  Estimated effects may be unbalanced
  1 observation deleted due to missingness
  >   summary(TimetoRecurrencebyRBC_age_group)
  Df Sum Sq Mean Sq F value Pr(>F)
  RBC.Age.Group   1    0.2  0.1835   0.097  0.755
  Residuals     313  590.9  1.8880               
  1 observation deleted due to missingness
  >   broom::tidy(TimetoRecurrencebyRBC_age_group)
  # A tibble: 2 x 6
  term             df   sumsq meansq statistic p.value
  <chr>         <dbl>   <dbl>  <dbl>     <dbl>   <dbl>
    1 RBC.Age.Group     1   0.184  0.184    0.0972   0.755
  2 Residuals       313 591.     1.89    NA       NA    
  
  
  
  
  
  
  
  
