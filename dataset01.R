### Data Cleaning ###

## Package Loading
library(knitr)
library(formattable)
library(tidyverse)

## Data Load ##
census = read.table(
    "census-population-and-housing-linc.csv",
    header = T,
    sep = ";",
    dec = "."
)
count = read.table(
  "",
  header = T,
  sep = ";",
  dec = ".",
  quote = "\"'"
)
education = read.table(
  "education.csv",
  header = T,
  sep = ";",
  dec = ".",
  quote = "\"'"
)
employment = read.table(
  "employment-and-income-linc.csv",
  header = T,
  sep = ";",
  dec = ".",
  quote = "\"'"
)

## FORMAT raw col as rows ##
census1 = tidyr::spread(census,3,4) #redo spread correctly, null values in pop est.
census1 = census1[-2]
census1 = na.omit(census1) #removes entries with null values

count1 = tidyr::spread(count,2,3)
count1 = count1[c(-2,-3)]

education1 = tidyr::spread(employment,3,4)
education1 = education1[-2]

employment1 = tidyr::spread(education,3,4)
employment1 = employment1[-2]

## JOIN datasets together ##
cc_table = inner_join(census1,count1,by='Area Name') # join edu and count
ee_table = inner_join(education1,employment1,by='Area Name')
full = inner_join(cc_table,ee_table,by='Area Name')

## New Variables ##
# dummy variable for race in full 
full$`Majority White`= ifelse(full$`White alone`>= 67.0,1,0)
full$`Majority White` = as.logical()
full1 = full # copy full to full1
full = full[c(-5,-6,-7,-8,-9,-10,-11,-12,-13)] # drop race variables
# seperate variables for latitude/longitude
fullc = separate(full1, geo_point_2d, sep=",", into = c("lat","long"))
fullc$lat = as.numeric(fullc$lat)
fullc$long = as.numeric(fullc$long)

names(full)
names(full1)
head(full,20)

## fullset as a matrix ##
# make a matrix (fullmatrix) from full [the hard way]
# full_m = full1[c(-1,-5,-6,-7,-8,-9,-10,-11,-12,-21)]
# fullm = as.matrix(sapply(full_m, as.numeric))
# the easy way
fullm = as.matrix(fullset[c(-1,-12,-13)])
class(fullm)

## transformed dataset ##
fullset = read.table(
  "fullset.csv",
  header = T,
  sep = ",",
  dec = ".",
  quote = "\"'"
)
var_names = names(fullset)
var_names = var_names[-1]
str(fullset)

## rename variables ##
fullset = fullset[,-1]
fullset = fullset %>% 
  rename(
    area = 1,
    medi_owner = 2,
    outsiders = 3,
    pop = 4,
    avg_employ = 5,
    avg_wage = 6,
    employ_resid = 7,
    fam_income = 8,
    college = 9,
    high_sch = 10,
    sat = 11,
    white_maj = 12,
    part = 13
  ) %>% 
  select(area, everything()) 
fullset$area = as.character(area)
str(fullset)

## export to .csv ##
write.csv(fullset, "C:\\Users\\Christian Okoth\\Documents\\EC 351\\LINC\\fullset.csv")
write.csv(fullm.r,"C:\\Users\\Christian Okoth\\Documents\\EC 351\\LINC\\fullset_corr.csv")
write.csv(fullm.p,"C:\\Users\\Christian Okoth\\Documents\\EC 351\\LINC\\fs_pvalues.csv")
write.csv(summ_stats,"C:\\Users\\Christian Okoth\\Documents\\EC 351\\LINC\\fullset_summary.csv")

# library(gridExtra)
# pdf("fullset_corr.pdf", height=11, width=8.5)
# grid.table(fullm.r)
# dev.off()
# 
# png("test.png", height = 50*nrow(df), width = 200*ncol(df))
# grid.table(fullm.r)
# dev.off()
# 
# grid.arrange(tableGrob(fullm.r, gp=gpar(fontsize=6)), main="Correlation Matrix")

# variable name fixes
## this needs to be done but not tonight
## done!
fullset = full
colnames(fullset)
fullset1 = fullset

# getting rid of the word "County"
# fullc2 = separate(fullc, fullc$`Area Name`, sep = " ", into = c("name","county"))
fullc = separate(
  full1, 
  geo_point_2d, 
  sep=",", 
  into = c("lat","long")
  )
# this method drops Hanover in New Hanover County [65,]
fullset$county = full$`Area Name`
fullset$county
fullset1 = fullset %>%
  separate(area, c("area","other"), extra = "merge", fill = "left")
fullset1[65,]

rlang::last_error()

## renaming df/mat ##
summ_stats = full_desc;pvalues = fullm.p;rvalues = fullm.r;matrix = fullm

## clearing environment ##
rm(full1,full_m,fullm,full_desc,full_measures)