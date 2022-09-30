library(tidyverse)
library(dplyr)

### Perform Multiple Regression on MechaCar dataset

# Read in the csv file.
mecha_data <- read.csv("MechaCar_mpg.csv",stringsAsFactors = F,check.names = F)

# Perform a linear regression module 
mecha_lm <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mecha_data)

# Determine the p-value and r-squared of the linear regression module.
summary <- summary(mecha_lm)


Suspension <- read.csv("Suspension_Coil.csv",stringsAsFactors = F,check.names = F)

totalsummary <- Suspension %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))

lotsummary <- Suspension %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI),.groups = 'keep')

t.test(Suspension$PSI,mu=1500)

t.test(subset(Suspension,Manufacturing_Lot=="Lot1")$PSI,mu=1500)
t.test(subset(Suspension,Manufacturing_Lot=="Lot2")$PSI,mu=1500)
t.test(subset(Suspension,Manufacturing_Lot=="Lot3")$PSI,mu=1500)
