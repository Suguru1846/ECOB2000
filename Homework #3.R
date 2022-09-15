library(data.table)
library(tidyverse)
library(DT)
library(dplyr)
library(tidyr)
library(psych)
options(dplyr.summarise.inform = FALSE)

load("/Household_Pulse_data.RData")

colnames(Household_Pulse_data)
df <- Household_Pulse_data
attach(df)

# My hypothesis is that people who have children got the vaccine more than people who don't have children. In addition, people who have children also went to restaurants less.
# I split the tables into KIDS_LT5Y, KIDS_5_11Y, KIDS_12_17Y, and people who don't have children to compare the proportion.

# 77.7 % of people who have children under 5 got the vaccine. 
# 79.2 % of people who have children 5 - 11 years old got the vaccine. 
# 80.2 % of people who have children 12 - 17 years old got the vaccine. 
# 90.1 % of people who don't have children got the vaccine. 

#In conclusion, having young children does not motivate people to get the vaccine. Age is a stronger factor. 

#Children under 5 in HH and vaccine status
KIDS_LT5Y_RECVDVACC <- df %>%
  group_by(KIDS_LT5Y,RECVDVACC) %>%
  summarize(n = n())

YES_KIDS_LT5Y_RECVDVACC <- KIDS_LT5Y_RECVDVACC[KIDS_LT5Y_RECVDVACC$KIDS_LT5Y == "Yes children under 5 in HH",]
YES_KIDS_LT5Y_RECVDVACC$prop <- prop.table(YES_KIDS_LT5Y_RECVDVACC$n)

#Children 5 - 11 in HH and vaccine status
KIDS_5_11Y_RECVDVACC <- df %>%
  group_by(KIDS_5_11Y,RECVDVACC) %>%
  summarize(n = n())

YES_KIDS_5_11Y_RECVDVACC <- KIDS_5_11Y_RECVDVACC[KIDS_5_11Y_RECVDVACC$KIDS_5_11Y == "Yes children 5 - 11 in HH",]
YES_KIDS_5_11Y_RECVDVACC$prop <- prop.table(YES_KIDS_5_11Y_RECVDVACC$n)

#Children 12 - 17 in HH and vaccine status
KIDS_12_17Y_RECVDVACC <- df %>%
  group_by(KIDS_12_17Y,RECVDVACC) %>%
  summarize(n = n())

YES_KIDS_12_17Y_RECVDVACC <- KIDS_12_17Y_RECVDVACC[KIDS_12_17Y_RECVDVACC$KIDS_12_17Y == "Yes children 12 - 17 in HH",]
YES_KIDS_12_17Y_RECVDVACC$prop <- prop.table(YES_KIDS_12_17Y_RECVDVACC$n)

# People who don't have children
df2 <-  df %>% 
  filter(KIDS_LT5Y == "NA" & KIDS_5_11Y == "NA" & KIDS_12_17Y == "NA") 

no_kids_vaxx <- df2 %>% group_by(RECVDVACC) %>%
  summarize(n = n())

no_kids_vaxx$prop <- prop.table(no_kids_vaxx$n)

# It looks like there might be a negative correlation between a child's age and the proportion of people who eat at restaurants indoors.
#37.3 % of people who have children under 5 went to indoor restaurants and 50.8% did not.
#42.0 % of people who have children 5 - 11 years old went to indoor restaurants and 45.7% did not.
#47.2 % of people who have children 12 - 17 years old went to indoor restaurants and 40.2% did not.
#48.5 % of people who don't have children went to indoor restaurants and 41.8% did not.

#Children under 5 in HH and restaurant
KIDS_LT5Y_restaurant <- df %>%
  group_by(KIDS_LT5Y,eat_in_restaurant) %>%
  summarize(n = n())

YES_KIDS_LT5Y_restaurant <- KIDS_LT5Y_restaurant[KIDS_LT5Y_restaurant$KIDS_LT5Y == "Yes children under 5 in HH",]
YES_KIDS_LT5Y_restaurant$prop <- prop.table(YES_KIDS_LT5Y_restaurant$n)

#Children 5 - 11 in HH and restaurant
KIDS_5_11Y_restaurant <- df %>%
  group_by(KIDS_5_11Y,eat_in_restaurant) %>%
  summarize(n = n())

YES_KIDS_5_11Y_restaurant <- KIDS_5_11Y_restaurant[KIDS_5_11Y_restaurant$KIDS_5_11Y == "Yes children 5 - 11 in HH",]
YES_KIDS_5_11Y_restaurant$prop <- prop.table(YES_KIDS_5_11Y_restaurant$n)

#Children 12 - 17 in HH and restaurant
KIDS_12_17Y_restaurant <- df %>%
  group_by(KIDS_12_17Y,eat_in_restaurant) %>%
  summarize(n = n())

YES_KIDS_12_17Y_restaurant <- KIDS_12_17Y_restaurant[KIDS_12_17Y_restaurant$KIDS_12_17Y == "Yes children 12 - 17 in HH",]
YES_KIDS_12_17Y_restaurant$prop <- prop.table(YES_KIDS_12_17Y_restaurant$n)

# People who don't have kids
no_kids_restaurant <- df2 %>% group_by(eat_in_restaurant) %>%
  summarize(n = n())

no_kids_restaurant$prop <- prop.table(no_kids_restaurant$n)