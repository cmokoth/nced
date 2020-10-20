### Data Cleaning ###

## Package Loading
library(knitr)
library(tidyverse)
library(formattable)
library(tidyverse)
library(skimr)

## Data Load ##
census = read_delim(
  "LINC/census-population-and-housing-linc.csv",
  delim = ";"
)
count = read_csv(
  "LINC/ncccc-nc-complete-count-committee.csv"
)
education = read_delim(
  "LINC/education.csv",
  delim = ";"
)
employment = read_delim(
  "LINC/employment-and-income-linc.csv",
  delim = ";"
)

## FORMAT raw col as rows ##
census1 = census %>% 
  pivot_wider(names_from = Variable,values_from = Value) %>% 
  select(-Year) %>% 
  na.omit() #removes entries with null values

count1 = count %>% 
  pivot_wider(names_from = Race,values_from = value) %>% 
  select(-main_facet)

education1 = education %>% 
  pivot_wider(names_from = Variable,values_from = Value) %>% 
  select(-`Area Type`)

employment1 = employment %>% 
  pivot_wider(names_from = Variable,values_from = Value) %>% 
  select(-Year)

#### join datasets together ####
# cc_table = inner_join(census1,count1,by='Area Name') # join edu and count
# ee_table = inner_join(education1,employment1,by='Area Name')
# full = inner_join(cc_table,ee_table,by='Area Name')

full = inner_join(census1,count1,by='Area Name') %>% 
  inner_join(education1,by='Area Name') %>% 
  inner_join(employment1,by='Area Name') %>% 
  mutate(maj_white = as.logical(ifelse(`White alone`>= 67.0,1,0))) %>% 
  # condense all the race variables into one column
  nest(race = c(6:13)) %>% 
  # drop race variables
  select(-6:13,
         -Year) %>% 
  # seperate latitude/longitude
  separate(geo_point_2d, sep = ",", into = c("lat","long")) %>% 
  mutate(lat = as.numeric(lat),
         long = as.numeric(long)) %>% 
  # better variable names
  rename(area = 1,
    medi_owner = 2,
    outsiders = 4,
    pop = 3,
    sat = 7,
    avg_employ = 10,
    avg_wage = 12,
    employ_resid = 11,
    fam_income = 13,
    college = 8,
    high_sch = 9)
    


## New Variables ##

# getting rid of the word "County"
# fullc2 = separate(fullc, fullc$`Area Name`, sep = " ", into = c("name","county"))
fullc = separate(
  full1, 
  geo_point_2d, 
  sep=",", 
  into = c("lat","long")
)

## fullset as a matrix ##
# make a matrix (fullmatrix) from full [the hard way]
# full_m = full1[c(-1,-5,-6,-7,-8,-9,-10,-11,-12,-21)]
# fullm = as.matrix(sapply(full_m, as.numeric))
# the easy way
# fullm = as.matrix(fullset[c(-1,-12,-13)])
# class(fullm)

#### export to .csv ####
write.csv(fullset, "\\LINC\\fullset.csv")
write.csv(fullm.r,"\\LINC\\fullset_corr.csv")
write.csv(fullm.p,"\\LINC\\fs_pvalues.csv")
write.csv(summ_stats,"\\LINC\\fullset_summary.csv")

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

## renaming df/mat ##
summ_stats = full_desc;pvalues = fullm.p;rvalues = fullm.r;matrix = fullm

## clearing environment ##
rm(full1,full_m,fullm,full_desc,full_measures)