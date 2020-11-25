### Data Cleaning ###

## Package Loading
library(readr)
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
  # separate latitude/longitude
  separate(geo_point_2d, sep = ",", into = c("lat","long"),remove = TRUE) %>% 
  mutate(lat = as.numeric(lat),
         long = as.numeric(long)) %>% 
  nest(coordinates = c(lat,long)) %>% 
  select(-Year) %>% 
  # better variable names
  rename(area = 1,
    medi_owner = 2,
    outsiders = 4,
    population = 3,
    sat = 5,
    avg_employ = 8,
    avg_wage = 10,
    employ_resid = 9,
    fam_income = 11,
    college = 6,
    high_sch = 7)
  
    



