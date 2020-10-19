#project 2 deliverable

#Divide your sample into two sub-samples. 
#Discuss why you have chosen to divide the sample in this way and 
#what types of questions you will be able to answer by comparing the two sub-samples. 
library(magrittr)
library(dplyr)
library(ggplot2)

#Base the split on a logical division within your sample where there are intuitive diﬀerences 
#between the two groups.

##add fun colors!!!

#defining logical split
#by college/HS ratio
ratio = full$`College Graduates Age 25 Up`/full$`High School Graduates Age 25 Up`
plot(ratio)
boxplot(ratio)
summary(ratio)
hist(ratio)

#by population
par(mfrow=c(1,1))
plot(full$`Population Estimate (BEA per Capita Denominator)`)
summary(full$`Population Estimate (BEA per Capita Denominator)`)

#by education
ratio1 = full$`College Graduates Age 25 Up`/full$`Population Estimate (BEA per Capita Denominator)`
ratio2 = full$`High School Graduates Age 25 Up`/full$`Population Estimate (BEA per Capita Denominator)`

par(mfrow=c(1,2))
plot(ratio1)
plot(ratio2)

#by geographic position
plot(fullc$long)
summary(fullc$long)
full$`Part of the State` = ifelse(fullc$long < -79.29, "West","East")
fullc$`Part of the State` = ifelse(fullc$long < -79.29, "West","East")

#by SAT average score
plot(full$`SAT Grand Total Average Score`)
summary(full$`SAT Grand Total Average Score`) #find data on SAT measuring/corr wealth

which(fullset$`Majority White`==0)



#Test the hypothesis that your dependent variable is equal across the two samples. 
#source: https://uc-r.github.io/t_test#twosample
## H0: mu1-mu2 == 0  HA: mu1-mu2 != 0
#boxplot comparing E/W samples 
## add colors!!!
ggplot(full, aes(full$`Part of the State`,full$`Estimated Median Family Income(HUD)`))+
       labs(x = "Part of the State",y = "Median Family Income ($)")+
       ggtitle("Distribution of Income by Part of the State") +
  geom_boxplot()

#histogram comparing E/W samples
##need to add density fn on top
#ggplot(full, aes(full$`Estimated Median Family Income(HUD)`)) +
#  geom_histogram(aes(y=..density..), #changes the y axis to a density measure
#                 binwidth = 5000,
#    fill = "white", color = "grey30") +
#  facet_wrap(~ full$`Part of the State`)
#just histograms

#histograms with kernel plots overlay
## add colors!!!
ggplot(full, aes(full$`Estimated Median Family Income(HUD)`)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 5000,
                 fill = "white", color = "grey30") +
  geom_line(stat="density") +
  facet_wrap(~ full$`Part of the State`)

#barchart of all data with transparent color kernel overlay
ggplot(full, aes(x=full$`Median Value of Owner Occupied Units`)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=5000,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

## testing significance between two groups
## differences t-test
paired_t = t.test(fullset$fam_income ~ fullset$part, data = full)

print(paired_t)

##        Welch Two Sample t-test

## data:  full$`Estimated Median Family Income(HUD)` by full$`Part of the State`
## t = 0.95703, df = 88.356, p-value = 0.3412
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##   -1896.622  5420.622
## sample estimates:
##   mean in group East mean in group West 
## 54874              53112 


#What are the implications of your ﬁndings?
##there is no significant difference between the two groups as the p-value is .3412,
##  indicating a 34.12% chance of this happening by chance