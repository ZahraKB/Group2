cor(data3$PreopPSA,data3$t_stage, use = "pairwise.complete.obs")

cor_tstage_preopPSA<- cor.test(data3$t_stage, data3$PreopPSA)
cor_tstage_preopPSA
ggplot(data = data3, aes(x ="t_stage" , y = "PreopPSA")) + 
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) 


#Answer: the value of t.stage does not explain the value of PreopPSA
