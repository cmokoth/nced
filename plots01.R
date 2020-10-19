#some analysis of the data
#load 'full' dataset
install.packages("wesanderson")
# color palletes
library(RColorBrewer); library(wesanderson)
# pipes and stuff I need
library(tidyverse); library(ggplot2)
library(readr)
library(magrittr)
library(scales)
full <- read_csv("~/EC 351/LINC/full.xlsx")
View(full)

str(fullset)
## set the theme globably ##
# http://sape.inf.usi.ch/quick-reference/ggplot2/colour 
egg = theme_gray() + 
  theme(panel.grid = element_line(color = "dodgerblue4"),
      plot.background = element_rect(color = "white"))
theme_set(egg)

## boxplot for resp(Median Income) ##
fullset %>%
  ggplot(aes(y = fam_income)) +
  stat_boxplot(geom = "errorbar", color = "firebrick", size = 1) +
  geom_boxplot(fill = "salmon",color = "firebrick",width = .5) +
  labs(title = "Boxplot for Median Family Income", 
       y = "Family Income in USD") +
  coord_flip()

#this graph needs to be resized
fullset %>% 
  ggplot(aes(x = pop, y = fam_income)) +
  geom_point(aes(size = college, color = college)) +
  # scale_fill_brewer(palette = "RdYlBu") +
  scale_color_brewer(palette = "RdYlBu") # geom_text(label = area)

ggplot(full, aes(y=full$`Estimated Median Family Income(HUD)`)) +
  geom_boxplot() +
  geom_text(label=full$`Area Name`)

par(mfrow=c(1,1))
#scatterplots to test relationships + historgam/barchart with kernel overlay
#plot(full$`Population Estimate (BEA per Capita Denominator)`,full$`Estimated Median Family Income(HUD)`)
#^plot() is equvalent to following plot():
sg.medi_owner = 
  full$`Estimated Median Family Income(HUD)` %>% 
    plot(full$`Population Estimate (BEA per Capita Denominator)`,., 
         main="Population/Income Relationship",xlab="Population Estimate",ylab="Estimated Median Family Income")
view(sg.medi_owner)
#fullset$fam_income %>%
  ggplot(fullset, aes_(y=fullset$fam_income,x=fullset$pop)) +
    geom_point() 
    xlab = "Number of Persons"

#weak pos, outliers
#using ggplot to make this a little nicer
ggplot(full, aes(y= .,x=full$`Population Estimate (BEA per Capita Denominator)`)) +
  geom_point() %>%

#histograms with kernel plots overlay (base code for hist)
## add colors!!!

ggplot(full, aes(full$`Estimated Median Family Income(HUD)`)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 5000,
                 fill = "white", color = "grey30") +
  geom_line(stat="density") +
  facet_wrap(~ full$`Part of the State`)

#plots
par(mfrow=c(1,2))
#plot(full$`Persons Living Outside This County Five Years Ago`,full$`Estimated Median Family Income(HUD)`)
ggplot(full, aes(x=full$`Persons Living Outside This County Five Years Ago`,y=full$`Estimated Median Family Income(HUD)`)) +
  geom_point()
  
#hist(full$`Persons Living Outside This County Five Years Ago`)
ggplot(full, aes(full$`Persons Living Outside This County Five Years Ago`)) +
  geom_histogram(aes(y=..density..),
                 binwidth = 10000,
                 fill = "white", color = "grey30") +
  geom_line(stat="density") 
#weak pos, outliers

#plot(full$`College Graduates Age 25 Up`,full$`Estimated Median Family Income(HUD)`)
s = full$`College Graduates Age 25 Up` %>%
  ggplot(full, aes(x= full$`College Graduates Age 25 Up`,y=full$`Estimated Median Family Income(HUD)`)) +
  geom_point()
#hist(full$`Persons Living Outside This County Five Years Ago`)
#ggplot(full, aes(full$`Persons Living Outside This County Five Years Ago`)) +
#  geom_histogram(aes(y=..density..),
#                 #alpha = .3,
#                 binwidth = 10000,
#                 fill = "white", color = "grey30") +
#  geom_density(aes(y=.0045*..count..), colour="black", adjust=4) +
#  scale_y_continuous(labels = scientific)

# hist with normal curve on top
x <- full$`Persons Living Outside This County Five Years Ago`
h<-hist(x, breaks=10, col=gray, xlab="Persons Outside the County 5 Years Ago",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, lwd=2)

#weak pos, outliers
plot(full$`Median Value of Owner Occupied Units`,full$`Estimated Median Family Income(HUD)`)
#positive relationship
plot(full$`Average Annual Wage per Worker`,full$`Estimated Median Family Income(HUD)`)
#pos
plot(full$`Average Annual Employment by Place of Work`*full$`Average Annual Wage per Worker`,full$`Estimated Median Family Income(HUD)`)

plot(full$`Median Value of Owner Occupied Units`*full$`Average Annual Wage per Worker`, full$`Estimated Median Family Income(HUD)`)

plot(full$`Average Annual Employment by Place of Work`, full$`Estimated Median Family Income(HUD)`)
#skewed heavily
plot(full$`Employment by Place of Residence`, full$`Estimated Median Family Income(HUD)`)
#skewed
plot(full$`High School Graduates Age 25 Up`,full$`Estimated Median Family Income(HUD)`)
#pos, has a similar shape to college graduates
plot(full$`SAT Grand Total Average Score`,full$`Estimated Median Family Income(HUD)`)
#pos
plot(full$`Majority White`,full$`Estimated Median Family Income(HUD)`)
#no 
plot(full$`White alone`,full$`Estimated Median Family Income(HUD)`)
#pos


#visualizing data

plot(full$`College Graduates Age 25 Up`)
plot(full$`High School Graduates Age 25 Up`)
plot(full$`Persons Living Outside This County Five Years Ago`)

par(mfrow=c(1,2)) #this creates one plot with 2 panels
plot(full$`College Graduates Age 25 Up`, main="Number of College Graduates")
plot(full$`High School Graduates Age 25 Up`, main="Number of High School Graduates")

par(mfrow=c(1,2)) #this creates one plot with 2 panels
plot(full$`College Graduates Age 25 Up`, main="Number of College Graduates")
plot(full$`Persons Living Outside This County Five Years Ago`, main="Number of Non-Native Residents")

par(mfrow=c(1,1))

#which counties are the outliers for college graduates?
## Buncome (Asheville), Durham (Durham), Forsyth (Winston-Salem), Guilford (Greensboro)
## Mecklenburg (Charlotte), New Hanover (Wilmington), Wake (Raleigh)
which(full$`College Graduates Age 25 Up`>=50000)
full[c(11,32,34,41,60,65,92),]
boxplot(full$`College Graduates Age 25 Up`) #registers 11 outliers

#which counties are the outliers for high school graduates?
## Buncome (Asheville), Cumberland (Fayetteville), Forsyth (Winston-Salem), 
## Gaston (Gastonia) (a Charlotte satellite city)
## Guilford (Greensboro), Mecklenburg (Charlotte), Wake (Raleigh)
which(full$`High School Graduates Age 25 Up`>=40000)
full[c(11,26,34,36,41,60,92),]
boxplot(full$`High School Graduates Age 25 Up`) #registers 5 outliers

#which counties are the outliers for non-native residents?
## Cumberland (Fayetteville), Durham (Durham), Forsyth (Winston-Salem), Guilford (Greensboro)
## Mecklenburg (Charlotte), 
## Onslow (Jacksonville) (location of Camp Lejune), Wake (Raleigh)
which(full$`Persons Living Outside This County Five Years Ago`>=20000)
full[c(26,32,34,41,60,67,92),]
boxplot(full$`Persons Living Outside This County Five Years Ago`)#registers 11 outliers
