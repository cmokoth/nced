#project 2 deliverable
install.packages("car")
library(psych)
library(ggplot2)
library(Hmisc)
library(carData)

#Make a histogram for each of your continuous variables and 
#comment on the characteristics of each distribution, e.g., 
#symmetry, skewness, kurtosis, etc. 

par(mfrow=c(1,1))
#histograms for each variable to test normality and other assumptions

hist(fullset$`Median Value of Owner Occupied Units`,
     main="Median Value of Owner Occupied Units",
     xlab="US Dollars ($)")
#normal Q-Q plot for normality
qqPlot(fullset$`Median Value of Owner Occupied Units`,
       main= "Normal Q-Q Plot: \nMedian Value of Owner Occupied Units",
       ylab = "Median Value of Owner Occupied Units")
plot(fullset$`Median Value of Owner Occupied Units`,fullset$`Estimated Median Family Income(HUD)`)
#slightly pos skewed near normal, symmetrical, outliers [index 68, 28]

hist(fullset$`Persons Living Outside This County Five Years Ago`,
     main="Persons Living Outside \nThis County Five Years Ago",
     xlab="Number of People")
qqPlot(fullset$`Persons Living Outside This County Five Years Ago`,
       main="Normal Q-Q Plot: Persons Living Outside \nThis County Five Years Ago",
       ylab="Number of People")
#plot(fullset$`Persons Living Outside This County Five Years Ago`,fullset$`Estimated Median Family Income(HUD)`)
#pos skewed, outliers

hist(fullset$`Estimated Median Family Income(HUD)`,
     main="Estimated Median Family Income(HUD)",
     xlab="US Dollars ($)")
hist(fullset$`Population Estimate (BEA per Capita Denominator)`,
     main="Population Estimate",
     xlab="Population Estimate")
#plot(fullset$`Population Estimate (BEA per Capita Denominator)`,fullset$`Estimated Median Family Income(HUD)`)
#pos skewed, outliers
hist(fullset$`College Graduates Age 25 Up`,
     main="College Graduates Age 25 Up",
     xlab="Number of People")
plot(fullset$`College Graduates Age 25 Up`,fullset$`Estimated Median Family Income(HUD)`)
#boxplot(fullset$`College Graduates Age 25 Up`)
#pos skewed, outliers
hist(fullset$`Average Annual Wage per Worker`)
plot(fullset$`Average Annual Employment by Place of Work`,fullset$`Estimated Median Family Income(HUD)`)
#slightly pos skewed, symetrical, outliers
hist(fullset$`High School Graduates Age 25 Up`,
     main="High School Graduates Age 25 Up",
     xlab = "Number of People")
plot(fullset$`High School Graduates Age 25 Up`,fullset$`Estimated Median Family Income(HUD)`)
#pos skewed
hist(fullset$`SAT Grand Total Average Score`,
     main="SAT Grand Total Average Score",
     xlab="Score (Points)")
plot(fullset$`SAT Grand Total Average Score`,fullset$`Estimated Median Family Income(HUD)`)
#normal symmetric slightly neg skew 
barplot(fullset$`Majority White`)
ggplot(fullset,aes(fullset$`Majority White`))+
  geom_bar()+
  ggtitle("Majority White")+
  labs(x="Proportion White (1 = Yes)")
#increasing?
ggplot(fullset,aes(fullset$`Part of the State`))+
  geom_bar()+
  ggtitle("Part of the State")+
  labs(x="Part of the State")
#

#Make a bar chart for each of your dummy variables and 
#comment on the proportion of your sample possessing that characteristic.
hist(fullset$`Majority White`)
#about 65% majority white, 35% not majority white. 
#before encoding, the white alone variable fell in a relatively linear form

#For continuous variables, provide measures of central location (mean, median, mode), 
#standard deviation, skewness and kurtosis. 
#ref: https://www.statmethods.net/stats/descriptives.html

#Also, for each variable, detail which of the measures are most useful 
#given the distribution of the variable.
#Use numerical techniques to describe each quantitative variable. 
summary(fullset$`Median Value of Owner Occupied Units`)

describe(fullset$`Median Value of Owner Occupied Units`)#mean/sd/med/skew/kurt/se
boxplot(fullset$`Median Value of Owner Occupied Units`)

fullset_desc = as.data.frame(psych::describe(fullset_m))


#plots

#Present a table of correlation coefficients for all of your variables. 
#Which correlations are statistically significant? 
## print fullm.r and fullm.r and perform analysis (good opportunity to learn heatmaps)

fullset.corr = cor(fullset)
print(as.matrix(fullm.r))
fullm = as.matrix(fullset[c(-1,-12,-13)])

#pearson correlation matrix
fullm.rcorr = rcorr(fullm)
fullm.rcorr
#print correlation matrix of r values
fullm.r = as.matrix(fullm.rcorr$r)
#print correlation matrix significance (p) levels
fullm.p = as.matrix(fullm.rcorr$P)

#Based on this, which of your independent variables seem likely to be important 
#determinants of your dependent variable.