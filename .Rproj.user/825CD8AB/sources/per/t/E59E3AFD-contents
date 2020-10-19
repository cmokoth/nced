#project 3 deliverable
install.packages("pander")
library(MASS)
library(pander)
library(stargazer)

#Run a regression that explores the relationship between your dependent variable and 
#all of your independent variables. 
colnames(fullset)

## Multiple Linear Regression Example
mlr_fs <- lm(fullset$`Estimated Median Family Income(HUD)` ~ x1 + x2 + x3, data=mydata)
summary(fit) 

full_aov = aov(fam_income ~ 
                 medi_owner +  outsiders  +  pop +  avg_employ +  avg_wage + employ_resid  + 
                 college + high_sch + sat + white_maj +  part, 
               data=fullset,
               y=TRUE)
summary(full_aov) # show results

#lm model
lin = lm(fam_income ~ 
            medi_owner +  outsiders  +  pop +  avg_employ +  avg_wage + employ_resid  + 
            college + high_sch + sat + white_maj +  part, 
          data=fullset,
          y=TRUE)
summary(lin)

summary(lin)$adj.r.squared

#rlm model
rlin = rlm(fam_income ~ 
             medi_owner +  outsiders  +  pop +  avg_employ +  avg_wage + employ_resid  + 
             college + high_sch + sat + white_maj +  part, 
           data=fullset,
           y=TRUE)
summary(rlin)

help("summary.rlm")
help("summary.aov")

#Interpret the regression by providing a verbal explanation for all of the regression output 
#provided by gretl. 
#Make precise statements about what your results tell you, being sure to 
#note which coefficients are statistically significant and which are not.
## F-test
{{2.53e+09-2.33e+09}/4}/{2.33e+09/{88}}

0.675851 - 0.687691

stargazer(gr.model.3)

stargazer(lin,lin1)
#Do the regression results support each of your predicted relationships 
#Explain. What did you ﬁnd that was surprising?
#Do the regression results support or refute each of the correlations
#Explain why or why not. Relate your discussion to our class discussion of omitted variable bias.
#How well does the regression ﬁt the data? 
#Discuss whether your model is appropriate given how well it does or does not ﬁt the data.

dist_fam_inc = hist(fullset$fam_income, xlab="Income", main="Family Income Distribution")#Check the required conditions for regressions by performing residual analysis. 
#Check for nonnormality of the error term, heteroskedasticity, and outliers 
#by examining the residuals from your regression.
#Check for multicollinearity as well. Discuss what you ﬁnd.
png