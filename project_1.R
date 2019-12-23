
install.packages("tidyverse")

census = census_population_and_housing_linc #shorten the name

#format col as rows
census1 = tidyr::spread(census,3,4) #redo spread correctly,null values in pop est.
census1 = census1[-2]
census1 = na.omit(census1) #removes entries with null values

count1 = tidyr::spread(count,2,3)
count1 = count1[c(-2,-3)]

education1 = tidyr::spread(employment,3,4)
education1 = education1[-2]

employment1 = tidyr::spread(education,3,4)
employment1 = employment1[-2]

#join datasets together
cc_table = inner_join(census1,count1,by='Area Name')
ee_table = inner_join(education1,employment1,by='Area Name')

full = inner_join(cc_table,ee_table,by='Area Name')

#scatterplots to test relationships
plot(full$`Population Estimate (BEA per Capita Denominator)`,full$`Estimated Median Family Income(HUD)`)
#weak pos, outliers
plot(full$`Persons Living Outside This County Five Years Ago`,full$`Estimated Median Family Income(HUD)`)
#weak pos, outliers
plot(full$`College Graduates Age 25 Up`,full$`Estimated Median Family Income(HUD)`)
#weak pos, outliers
plot(full$`Median Value of Owner Occupied Units`,full$`Estimated Median Family Income(HUD)`)
#positive relationship
plot(full$`Average Annual Wage per Worker`,full$`Estimated Median Family Income(HUD)`)
#pos
plot(full$`High School Graduates Age 25 Up`,full$`Estimated Median Family Income(HUD)`)
#pos
plot(full$`SAT Grand Total Average Score`,full$`Estimated Median Family Income(HUD)`)
#weak pos
plot(full$`White alone`,full$`Estimated Median Family Income(HUD)`)
#no 
plot(full$`Black or African American alone`,full$`Estimated Median Family Income(HUD)`)
#weak neg
plot(full$`Asian alone`,full$`Estimated Median Family Income(HUD)`)
#weak pos
plot(full$`Hispanic or Latino (of any race)`,full$`Estimated Median Family Income(HUD)`)
#no
plot(full$`Two or more races`,full$`Estimated Median Family Income(HUD)`)
#no

#print
head(full,20)

#export to .xlsx
install.packages("xlsx")
library(xlsx)
write_excel_csv(full, "C:\\Users\\Christian Okoth\\Documents\\EC 351\\LINC\\full.xlsx")

?write_excel_csv
