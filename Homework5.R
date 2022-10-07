#Suguru Iwashiro
#10/5/2022
#Group members
#Adeel Arshid, Amira Elmakawy

#I coded two type of multiple linear regression models to test for individual income status. One model checks very basic demographic status. I picked age, gender, race, and veteran status as the independent variables. As a result, being white or asian is a huge factor for individual income. Gender (in this case, being a woman) and being a veteran has a negative impact. 
#Second, I tested the factors on income by using the status of the degree–educ_nohs, educ_hs, educ_college,educ_advdeg–and I made dummy variables of an economics degree (Economics_deg) and a business degree (business_deg), which I thought would have an impact on income.
#As a result, masters degrees (educ_advdeg) and economics degrees (Economics_deg) have a huge impact on income. I am glad that a masters degree in economics seems to have a great positive impact on individual income.

library(data.table)
library(ggplot2)
library(tidyverse)
library(DT)
library(dplyr)
library(tidyr)
library(psych)
options(dplyr.summarise.inform = FALSE)

setwd("/Users/Suguru/Desktop/Econometrics")
load("acs2017_ny/acs2017_ny_data.RData")

rm(list = ls(all = TRUE))
colnames(acs2017_ny)
attach(acs2017_ny)

use_varb <- (AGE >= 22) & (AGE <= 65) & (LABFORCE == 2) & (WKSWORK2 > 0) & (UHRSWORK >= 1)
dat_use <- subset(acs2017_ny,use_varb) 
attach(dat_use)
VETSTAT
veteran <- ifelse(VETSTAT == 2, 1, 0)

Economics_deg <- ifelse(DEGFIELDD == "Economics", 1, 0)
business_deg <- ifelse(DEGFIELDD == "Business Management and Administration" | DEGFIELDD == "General Business", 1, 0)

model_temp1 <- lm(INCTOT ~  AGE + female + white + AfAm + Asian + veteran)
model_temp2 <- lm(INCTOT ~  educ_nohs + educ_hs + educ_college + educ_advdeg + Economics_deg + business_deg)

summary(model_temp1)
summary(model_temp2)

plot(model_temp1)
plot(model_temp2)

install.packages("stargazer")
library(stargazer)
require(stargazer)
stargazer(model_temp1, type = "text")