#project 3 deliverable

#Create a nonlinear function of a single independent variable or an interaction term. 
#A popular choice is the square of one of your regressors. 
## logarithm of population variable
fullset$log_pop = log(fullset$pop)
fullc$`Logarithm of Population` = fullset$log_pop

fullset$int_own_wage = fullset$medi_owner*fullset$avg_wage
fullc$`Median Value of Units Annual Wage Interaction` = fullset$int_own_wage

par(mfrow = 1,2)
s.pop = ggplot(fullset, aes(y=fullset$fam_income, x=fullset$pop))+
  geom_point()

s.log_pop = ggplot(fullset, aes(y=fullset$fam_income, x=fullset$log_pop)) +
  geom_point()


s.pop
s.log_pop
#Pick which X to use by looking at scatterplots in PA#2 to 
#ﬁnd a relationship that appears nonlinear. 

#Discuss why controlling for this type of nonlinearity is important 
#and might improve your results. 
lin = lm(fam_income ~ 
           medi_owner +  outsiders  +  pop +  avg_employ +  avg_wage + 
           employ_resid  + college + high_sch + sat + white_maj +  part, 
         data=fullset,
         y=TRUE)
summary(lin)
#Rerun your regression model with the nonlinear term(s) included. 
lin1 = lm(fam_income ~ 
           medi_owner +  outsiders  +  pop + log_pop + avg_employ +  avg_wage + employ_resid + 
           college + high_sch + sat + white_maj +  part + int_own_wage, 
         data=fullset,
         y=TRUE)
summary(lin1)
#Discuss any changes in the results you observed. 
#Which model is preferred?