library(data.table)
library(ggplot2)
library(tidyverse)
library(DT)
library(dplyr)
library(tidyr)
library(psych)
library(standardize)
library(kableExtra)
library("epiDisplay", warn.conflicts=FALSE, quietly=TRUE)
options(dplyr.summarise.inform = FALSE)
rm(list = ls(all = TRUE))

setwd("/Users/Suguru/Desktop/Econometrics/final")

Adult21 <- read.csv("adult21.csv", stringsAsFactors=FALSE)
Adult20 <- read.csv("adult20.csv", stringsAsFactors=FALSE)

attach(Adult21)
attach(Adult20)
ls(Adult21)

#2021 anxeity
use_varb1 <- ((ANXFREQ_A == 1) | (ANXFREQ_A == 2) | (ANXFREQ_A == 3) | (ANXFREQ_A == 4) | 
                (ANXFREQ_A == 5)) & (AGEP_A <= 84) & (WEIGHTLBTC_A <= 299) & (HEIGHTTC_A <= 76)
df <- subset(Adult21, use_varb1)

#2020 anxeity
use_varb2 <- ((ANXFREQ_A == 1) | (ANXFREQ_A == 2) | (ANXFREQ_A == 3) | (ANXFREQ_A == 4) | 
                (ANXFREQ_A == 5)) & (AGEP_A <= 84) & (WEIGHTLBTC_A <= 299) & (HEIGHTTC_A <= 76)
df20 <- subset(Adult20, use_varb2)

#2021
df_anx <- df %>%
  dplyr::select(REGION,
         AGEP_A,
         SEX_A,
         EDUCP_A,
         PHSTAT_A, #Health Status
         HYPEV_A, #high blood pressure
         CHLEV_A,
         CHDEV_A,
         ASEV_A,
         CANEV_A,
         PREDIB_A,
         HEARAID_A,
         DIFF_A,
         COMDIFF_A,
         COGMEMDFF_A,
         NOTCOV_A,
         PAYBLL12M_A,
         CVDDIAG_A,
         RX12M_A,
         ANXFREQ_A,
         DEPFREQ_A,
         PAIFRQ3M_A,
         ANYINJURY_A,
         SMKEV_A,
         SUPPORT_A,
         ORIENT_A,
         MARITAL_A,
         AFVET_A,
         WEIGHTLBTC_A,
         HEIGHTTC_A)

#2020
df_anx20 <- df20 %>%
  dplyr::select(REGION,
                AGEP_A,
                SEX_A,
                EDUC_A,
                PHSTAT_A, #Health Status
                HYPEV_A, #high blood pressure
                CHLEV_A,
                CHDEV_A,
                ASEV_A,
                CANEV_A,
                PREDIB_A,
                HEARAID_A,
                DIFF_A,
                COMDIFF_A,
                COGMEMDFF_A,
                NOTCOV_A,
                PAYBLL12M_A,
                CVDDIAG_A,
                RX12M_A,
                ANXFREQ_A,
                DEPFREQ_A,
                PAIFRQ3M_A,
                ANYINJURY_A,
                SMKEV_A,
                SUPPORT_A,
                ORIENT_A,
                MARITAL_A,
                AFVET_A,
                WEIGHTLBTC_A,
                HEIGHTTC_A)

df_anx20$CVDDIAG_A <- df_anx20$CVDDIAG_A %>% replace_na(8)
df_anx20$SUPPORT_A <- df_anx20$SUPPORT_A %>% replace_na(8)

#2021 make dummy variables
d_REGION <- data.frame(model.matrix(~REGION-1, df_anx %>% mutate(REGION = factor(REGION))))
d_SEX_A <- data.frame(model.matrix(~SEX_A-1, df_anx %>% mutate(SEX_A = factor(SEX_A))))
d_COLLEGE <- data.frame(COLLEGE = ifelse((df_anx$EDUCP_A == 8 | df_anx$EDUCP_A == 9 | df_anx$EDUCP_A == 10), 1,0))
d_PHSTAT_A <- data.frame(PHSTAT_A = ifelse(df_anx$PHSTAT_A == 5, 1,0))
d_HYPEV_A <- data.frame(model.matrix(~HYPEV_A-1, df_anx %>% mutate(HYPEV_A = factor(HYPEV_A))))
d_CHLEV_A <- data.frame(model.matrix(~CHLEV_A-1, df_anx %>% mutate(CHLEV_A = factor(CHLEV_A))))
d_ASEV_A <- data.frame(model.matrix(~ASEV_A-1, df_anx %>% mutate(ASEV_A = factor(ASEV_A))))
d_CANEV_A <- data.frame(model.matrix(~CANEV_A-1, df_anx %>% mutate(CANEV_A = factor(CANEV_A))))
d_PREDIB_A <- data.frame(model.matrix(~PREDIB_A-1, df_anx %>% mutate(PREDIB_A = factor(PREDIB_A))))
d_HEARAID_A <- data.frame(model.matrix(~HEARAID_A-1, df_anx %>% mutate(HEARAID_A = factor(HEARAID_A))))
d_DIFF_A <- data.frame(DIFF_A = ifelse((df_anx$DIFF_A == 2 | df_anx$EDUCP_A == 3 | df_anx$EDUCP_A == 4), 1,0))
d_COMDIFF_A <- data.frame(model.matrix(~COMDIFF_A-1, df_anx %>% mutate(COMDIFF_A = factor(COMDIFF_A))))
d_COGMEMDFF_A <- data.frame(model.matrix(~COGMEMDFF_A-1, df_anx %>% mutate(COGMEMDFF_A = factor(COGMEMDFF_A))))
d_NOTCOV_A <- data.frame(model.matrix(~NOTCOV_A-1, df_anx %>% mutate(NOTCOV_A = factor(NOTCOV_A))))
d_PAYBLL12M_A <- data.frame(model.matrix(~PAYBLL12M_A-1, df_anx %>% mutate(PAYBLL12M_A = factor(PAYBLL12M_A))))
d_CVDDIAG_A <- data.frame(model.matrix(~CVDDIAG_A-1, df_anx %>% mutate(CVDDIAG_A = factor(CVDDIAG_A))))
d_RX12M_A <- data.frame(model.matrix(~RX12M_A-1, df_anx %>% mutate(RX12M_A = factor(RX12M_A))))
d_ANXFREQ_A <- data.frame(model.matrix(~ANXFREQ_A-1, df_anx %>% mutate(ANXFREQ_A = factor(ANXFREQ_A))))
d_DEPFREQ_A <- data.frame(model.matrix(~DEPFREQ_A-1, df_anx %>% mutate(DEPFREQ_A = factor(DEPFREQ_A))))
d_PAIFRQ3M_A <- data.frame(model.matrix(~PAIFRQ3M_A-1, df_anx %>% mutate(PAIFRQ3M_A = factor(PAIFRQ3M_A))))
d_ANYINJURY_A <- data.frame(model.matrix(~ANYINJURY_A-1, df_anx %>% mutate(ANYINJURY_A = factor(ANYINJURY_A))))
d_SMKEV_A <- data.frame(model.matrix(~SMKEV_A-1, df_anx %>% mutate(SMKEV_A = factor(SMKEV_A))))
d_SUPPORT_A <- data.frame(model.matrix(~SUPPORT_A-1, df_anx %>% mutate(SUPPORT_A = factor(SUPPORT_A))))
d_ORIENT_A <- data.frame(model.matrix(~ORIENT_A-1, df_anx %>% mutate(ORIENT_A = factor(ORIENT_A))))
d_MARITAL_A <- data.frame(model.matrix(~MARITAL_A-1, df_anx %>% mutate(MARITAL_A = factor(MARITAL_A))))
d_AFVET_A <- data.frame(model.matrix(~AFVET_A-1, df_anx %>% mutate(AFVET_A = factor(AFVET_A))))
GAD <-d_ANXFREQ_A[,1]

#2020 make dummy variables
d_REGION20 <- data.frame(model.matrix(~REGION-1, df_anx20 %>% mutate(REGION = factor(REGION))))
d_SEX_A20 <- data.frame(model.matrix(~SEX_A-1, df_anx20 %>% mutate(SEX_A = factor(SEX_A))))
d_COLLEGE20 <- data.frame(COLLEGE = ifelse((df_anx20$EDUC_A == 8 | df_anx20$EDUC_A == 9 | df_anx20$EDUC_A == 10), 1,0))
d_PHSTAT_A20 <- data.frame(PHSTAT_A = ifelse(df_anx20$PHSTAT_A == 5, 1,0))
d_HYPEV_A20 <- data.frame(model.matrix(~HYPEV_A-1, df_anx20 %>% mutate(HYPEV_A = factor(HYPEV_A))))
d_CHLEV_A20 <- data.frame(model.matrix(~CHLEV_A-1, df_anx20 %>% mutate(CHLEV_A = factor(CHLEV_A))))
d_ASEV_A20 <- data.frame(model.matrix(~ASEV_A-1, df_anx20 %>% mutate(ASEV_A = factor(ASEV_A))))
d_CANEV_A20 <- data.frame(model.matrix(~CANEV_A-1, df_anx20 %>% mutate(CANEV_A = factor(CANEV_A))))
d_PREDIB_A20 <- data.frame(model.matrix(~PREDIB_A-1, df_anx20 %>% mutate(PREDIB_A = factor(PREDIB_A))))
d_HEARAID_A20 <- data.frame(model.matrix(~HEARAID_A-1, df_anx20 %>% mutate(HEARAID_A = factor(HEARAID_A))))
d_DIFF_A20 <- data.frame(DIFF_A = ifelse((df_anx20$DIFF_A == 2 | df_anx20$DIFF_A == 3 | df_anx20$DIFF_A == 4), 1,0))
d_COMDIFF_A20 <- data.frame(model.matrix(~COMDIFF_A-1, df_anx20 %>% mutate(COMDIFF_A = factor(COMDIFF_A))))
d_COGMEMDFF_A20 <- data.frame(model.matrix(~COGMEMDFF_A-1, df_anx20 %>% mutate(COGMEMDFF_A = factor(COGMEMDFF_A))))
d_NOTCOV_A20 <- data.frame(model.matrix(~NOTCOV_A-1, df_anx20 %>% mutate(NOTCOV_A = factor(NOTCOV_A))))
d_PAYBLL12M_A20 <- data.frame(model.matrix(~PAYBLL12M_A-1, df_anx20 %>% mutate(PAYBLL12M_A = factor(PAYBLL12M_A))))
d_CVDDIAG_A20 <- data.frame(model.matrix(~CVDDIAG_A-1, df_anx20 %>% mutate(CVDDIAG_A = factor(CVDDIAG_A))))
d_RX12M_A20 <- data.frame(model.matrix(~RX12M_A-1, df_anx20 %>% mutate(RX12M_A = factor(RX12M_A))))
d_ANXFREQ_A20 <- data.frame(model.matrix(~ANXFREQ_A-1, df_anx20 %>% mutate(ANXFREQ_A = factor(ANXFREQ_A))))
d_DEPFREQ_A20 <- data.frame(model.matrix(~DEPFREQ_A-1, df_anx20 %>% mutate(DEPFREQ_A = factor(DEPFREQ_A))))
d_PAIFRQ3M_A20 <- data.frame(model.matrix(~PAIFRQ3M_A-1, df_anx20 %>% mutate(PAIFRQ3M_A = factor(PAIFRQ3M_A))))
d_ANYINJURY_A20 <- data.frame(model.matrix(~ANYINJURY_A-1, df_anx20 %>% mutate(ANYINJURY_A = factor(ANYINJURY_A))))
d_SMKEV_A20 <- data.frame(model.matrix(~SMKEV_A-1, df_anx20 %>% mutate(SMKEV_A = factor(SMKEV_A))))
d_SUPPORT_A20 <- data.frame(model.matrix(~SUPPORT_A-1, df_anx20 %>% mutate(SUPPORT_A = factor(SUPPORT_A))))
d_ORIENT_A20 <- data.frame(model.matrix(~ORIENT_A-1, df_anx20 %>% mutate(ORIENT_A = factor(ORIENT_A))))
d_MARITAL_A20 <- data.frame(model.matrix(~MARITAL_A-1, df_anx20 %>% mutate(MARITAL_A = factor(MARITAL_A))))
d_AFVET_A20 <- data.frame(model.matrix(~AFVET_A-1, df_anx20 %>% mutate(AFVET_A = factor(AFVET_A))))
GAD20 <-d_ANXFREQ_A20[,1]



#2021
dat_for_analysis <- data.frame(
  d_REGION,
  d_SEX_A[,1],
  d_COLLEGE,
  d_PHSTAT_A,
  d_HYPEV_A[,1],
  d_CHLEV_A[,1],
  d_ASEV_A[,1],
  d_CANEV_A[,1],
  d_PREDIB_A[,1],
  d_HEARAID_A[,1],
  d_DIFF_A,
  d_COMDIFF_A[,1:4],
  d_COGMEMDFF_A[,1:4],
  d_NOTCOV_A[,1],
  d_PAYBLL12M_A[,1],
  d_CVDDIAG_A[,1],
  d_RX12M_A[,1],
  GAD,
  d_DEPFREQ_A[,1:5],
  d_PAIFRQ3M_A[,1:4],
  d_ANYINJURY_A[,1],
  d_SMKEV_A[,1],
  d_SUPPORT_A[,1:5],
  d_ORIENT_A[,1:4],
  d_MARITAL_A[,1:3],
  d_AFVET_A[,1],
  df_anx$WEIGHTLBTC_A,
  df_anx$HEIGHTTC_A,
  df_anx$AGEP_A
  )

#2020
dat_for_analysis20 <- data.frame(
  d_REGION20,
  d_SEX_A20[,1],
  d_COLLEGE20,
  d_PHSTAT_A20,
  d_HYPEV_A20[,1],
  d_CHLEV_A20[,1],
  d_ASEV_A20[,1],
  d_CANEV_A20[,1],
  d_PREDIB_A20[,1],
  d_HEARAID_A20[,1],
  d_DIFF_A20,
  d_COMDIFF_A20[,1:4],
  d_COGMEMDFF_A20[,1:4],
  d_NOTCOV_A20[,1],
  d_PAYBLL12M_A20[,1],
  d_CVDDIAG_A20[,1],
  d_RX12M_A20[,1],
  GAD20,
  d_DEPFREQ_A20[,1:5],
  d_PAIFRQ3M_A20[,1:4],
  d_ANYINJURY_A20[,1],
  d_SMKEV_A20[,1],
  d_SUPPORT_A20[,1:5],
  d_ORIENT_A20[,1:4],
  d_MARITAL_A20[,1:3],
  d_AFVET_A20[,1],
  df_anx20$WEIGHTLBTC_A,
  df_anx20$HEIGHTTC_A,
  df_anx20$AGEP_A
)

#Name change
names(dat_for_analysis) <- sub("df_anx.","",names(dat_for_analysis))
names(dat_for_analysis)[5] <- "SEX"
names(dat_for_analysis)[8] <- "HYPEV"
names(dat_for_analysis)[9] <- "CHLEV"
names(dat_for_analysis)[10] <- "ASEV"
names(dat_for_analysis)[11] <- "CANEV"
names(dat_for_analysis)[12] <- "PREDIB"
names(dat_for_analysis)[13] <- "HEARAID"
names(dat_for_analysis)[23] <- "NOTCOV"
names(dat_for_analysis)[24] <- "PAYBLL12M"
names(dat_for_analysis)[25] <- "CVDDIAG"
names(dat_for_analysis)[26] <- "RX12M"
names(dat_for_analysis)[37] <- "ANYINJURY"
names(dat_for_analysis)[38] <- "SMKEV"
names(dat_for_analysis)[51] <- "AFVET"
names(dat_for_analysis20) <- sub("df_anx20.","",names(dat_for_analysis20))
names(dat_for_analysis20)[27] <- "GAD"
names(dat_for_analysis20)[5] <- "SEX"
names(dat_for_analysis20)[8] <- "HYPEV"
names(dat_for_analysis20)[9] <- "CHLEV"
names(dat_for_analysis20)[10] <- "ASEV"
names(dat_for_analysis20)[11] <- "CANEV"
names(dat_for_analysis20)[12] <- "PREDIB"
names(dat_for_analysis20)[13] <- "HEARAID"
names(dat_for_analysis20)[23] <- "NOTCOV"
names(dat_for_analysis20)[24] <- "PAYBLL12M"
names(dat_for_analysis20)[25] <- "CVDDIAG"
names(dat_for_analysis20)[26] <- "RX12M"
names(dat_for_analysis20)[37] <- "ANYINJURY"
names(dat_for_analysis20)[38] <- "SMKEV"
names(dat_for_analysis20)[51] <- "AFVET"

ddd <-dat_for_analysis %>% 
  dplyr::select(GAD, COGMEMDFF_A4)
ddd[COGMEMDFF_A4 == 1,]

#Proportion DEPFREQ
xtabs(~GAD+DEPFREQ_A1, dat_for_analysis)
xtabs(~GAD+DEPFREQ_A2, dat_for_analysis)
xtabs(~GAD+DEPFREQ_A3, dat_for_analysis)
xtabs(~GAD+DEPFREQ_A4, dat_for_analysis)
xtabs(~GAD+DEPFREQ_A5, dat_for_analysis)

xtabs(~GAD+DEPFREQ_A1, dat_for_analysis20)
xtabs(~GAD+DEPFREQ_A2, dat_for_analysis20)
xtabs(~GAD+DEPFREQ_A3, dat_for_analysis20)
xtabs(~GAD+DEPFREQ_A4, dat_for_analysis20)
xtabs(~GAD+DEPFREQ_A5, dat_for_analysis20)

#COGMEMDFF
xtabs(~GAD+COGMEMDFF_A4, dat_for_analysis)
xtabs(~GAD+COGMEMDFF_A3, dat_for_analysis)
xtabs(~GAD+COGMEMDFF_A2, dat_for_analysis)
xtabs(~GAD+COGMEMDFF_A1, dat_for_analysis)

xtabs(~GAD+COGMEMDFF_A4, dat_for_analysis20)
xtabs(~GAD+COGMEMDFF_A3, dat_for_analysis20)
xtabs(~GAD+COGMEMDFF_A2, dat_for_analysis20)
xtabs(~GAD+COGMEMDFF_A1, dat_for_analysis20)

#Data Means
demog <- dat_for_analysis %>% 
  dplyr::select(SEX, AGEP_A, WEIGHTLBTC_A, HEIGHTTC_A, GAD)

demog20 <- dat_for_analysis20 %>% 
  dplyr::select(SEX, AGEP_A, WEIGHTLBTC_A, HEIGHTTC_A, GAD)

#2021
gad_all <- demog[demog$GAD == 1,]
not_gad_all <- demog[demog$GAD != 1,]
men <- demog[demog$SEX == 1,]
men_gad <- men[men$GAD == 1,]
men_not_gad <- men[men$GAD!= 1,]
women <- demog[demog$SEX == 0,]
women_gad <- women[women$GAD == 1,]
women_not_gad <- women[women$GAD != 1,]

#2020
gad_all20 <- demog20[demog20$GAD == 1,]
not_gad_all20 <- demog20[demog20$GAD != 1,]
men20 <- demog20[demog20$SEX == 1,]
men_gad20 <- men20[men20$GAD == 1,]
men_not_gad20 <- men20[men20$GAD!= 1,]
women20 <- demog20[demog20$SEX == 0,]
women_gad20 <- women20[women20$GAD == 1,]
women_not_gad20 <- women20[women20$GAD != 1,]

demog$GAD <- as.factor(demog$GAD)

gad_all_box <- ggplot(demog)+
  geom_boxplot(aes(x=GAD,y= AGEP_A,fill = GAD))+
  ylab("AGE")+
  theme(legend.position = "none")+ 
  ggtitle("2021 Overall")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("gad_all_box.jpg")

men_box <- ggplot(men)+
  geom_boxplot(aes(x=GAD,y= AGEP_A,fill = GAD))+
  ylab("AGE")+
  theme(legend.position = "none")+ 
  ggtitle("2021 Men's Age")+
  theme(plot.title = element_text(hjust = 0.5))
men_box
ggsave("men.jpg")

women_box <- ggplot(women)+
  geom_boxplot(aes(x=GAD,y= AGEP_A,fill = GAD))+
  ylab("AGE")+
  theme(legend.position = "none")+ 
  ggtitle("2021 Women's Age")+
  theme(plot.title = element_text(hjust = 0.5))
women_box
ggsave("women.jpg")

gad_all_box20 <- ggplot(demog20)+
  geom_boxplot(aes(x=GAD,y= AGEP_A,fill = GAD))+
  ylab("AGE")+
  theme(legend.position = "none")+ 
  ggtitle("2020 Overall")+
  theme(plot.title = element_text(hjust = 0.5))
gad_all_box20
ggsave("gad_all_box20.jpg")

men_box20 <- ggplot(men20)+
  geom_boxplot(aes(x=GAD,y= AGEP_A,fill = GAD))+
  ylab("AGE")+
  theme(legend.position = "none")+ 
  ggtitle("2020 Men's Age")+
  theme(plot.title = element_text(hjust = 0.5))
men_box20
ggsave("men20.jpg")

women_box20 <- ggplot(women20)+
  geom_boxplot(aes(x=GAD,y= AGEP_A,fill = GAD))+
  ylab("AGE")+
  theme(legend.position = "none")+ 
  ggtitle("2020 Women's Age")+
  theme(plot.title = element_text(hjust = 0.5))
women_box20
ggsave("women20.jpg")

gad_all_box
men_box <- ggplot(men)+geom_boxplot(aes(x=GAD,y= AGEP_A, fill="red"))
men_box
women_box <- ggplot(women)+geom_boxplot(aes(x=GAD,y= AGEP_A, fill="red"))
women_box
summarize(demog, mean = mean(AGEP_A), sd = sd(AGEP_A))
summarize(demog20, mean = mean(AGEP_A), sd = sd(AGEP_A))
summarize(gad_all, mean = mean(AGEP_A), sd = sd(AGEP_A))
summarize(gad_all20, mean = mean(AGEP_A), sd = sd(AGEP_A))
summarize(not_gad_all, mean = mean(AGEP_A), sd = sd(AGEP_A))
summarize(not_gad_all20, mean = mean(AGEP_A), sd = sd(AGEP_A))
summarize(men_gad, mean = mean(AGEP_A), sd = sd(AGEP_A))
summarize(men_gad20, mean = mean(AGEP_A), sd = sd(AGEP_A))
summarize(men_not_gad, mean = mean(AGEP_A), sd = sd(AGEP_A))
summarize(men_not_gad20, mean = mean(AGEP_A), sd = sd(AGEP_A))
summarize(women_gad, mean = mean(AGEP_A), sd = sd(AGEP_A))
summarize(women_gad20, mean = mean(AGEP_A), sd = sd(AGEP_A))
summarize(women_not_gad, mean = mean(AGEP_A), sd = sd(AGEP_A))
summarize(women_not_gad20, mean = mean(AGEP_A), sd = sd(AGEP_A))

summarize(gad_all, n=n())
summarize(not_gad_all, n=n())
summarize(men_gad, n=n())
summarize(men_not_gad, n=n())
summarize(women_gad, n=n())
summarize(women_not_gad, n=n())

summarize(gad_all20, n=n())
summarize(not_gad_all20, n=n())
summarize(men_gad20, n=n())
summarize(men_not_gad20, n=n())
summarize(women_gad20, n=n())
summarize(women_not_gad20, n=n())

summary(gad_all)
summarize(gad_all, age_sd = sd(AGEP_A), weight_sd = sd(WEIGHTLBTC_A), height = sd(HEIGHTTC_A))
summary(not_gad_all)
summarize(not_gad_all, age_sd = sd(AGEP_A), weight_sd = sd(WEIGHTLBTC_A), height = sd(HEIGHTTC_A))

summary(men)
summary(men_gad)
summary(men_not_gad)
summarize(men, age_sd = sd(AGEP_A), weight_sd = sd(WEIGHTLBTC_A), height = sd(HEIGHTTC_A))
summarize(men_gad, age_sd = sd(AGEP_A), weight_sd = sd(WEIGHTLBTC_A), height = sd(HEIGHTTC_A))
summarize(men_not_gad, age_sd = sd(AGEP_A), weight_sd = sd(WEIGHTLBTC_A), height = sd(HEIGHTTC_A))

summary(women)
summary(women_gad)
summary(women_not_gad)

summarize(women, age_sd = sd(AGEP_A), weight_sd = sd(WEIGHTLBTC_A), height = sd(HEIGHTTC_A))
summarize(women_gad, age_sd = sd(AGEP_A), weight_sd = sd(WEIGHTLBTC_A), height = sd(HEIGHTTC_A))
summarize(women_not_gad, age_sd = sd(AGEP_A), weight_sd = sd(WEIGHTLBTC_A), height = sd(HEIGHTTC_A))

all_age <- t.test(x=gad_all$AGEP_A, y=not_gad_all$AGEP_A, paired = F, conf.level = 0.95, alternative = "two.side")
men_age <- t.test(x=men_gad$AGEP_A, y=men_not_gad$AGEP_A, paired = F, conf.level = 0.95, alternative = "two.side")
women_age <- t.test(x=women_gad$AGEP_A, y=women_not_gad$AGEP_A, paired = F, conf.level = 0.95, alternative = "two.side")

all_age20 <- t.test(x=gad_all20$AGEP_A, y=not_gad_all20$AGEP_A, paired = F, conf.level = 0.95, alternative = "two.side")
men_age20 <- t.test(x=men_gad20$AGEP_A, y=men_not_gad20$AGEP_A, paired = F, conf.level = 0.95, alternative = "two.side")
women_age20 <- t.test(x=women_gad20$AGEP_A, y=women_not_gad20$AGEP_A, paired = F, conf.level = 0.95, alternative = "two.side")

se <- function(a) sd(a) / sqrt(length(a))
men_gad_se<- se(men_gad$AGEP_A)
dat_for_analysis$WEIGHTLBTC_A

#change classes
dat_for_analysis[] <- lapply( dat_for_analysis, factor)
dat_for_analysis$WEIGHTLBTC_A <- as.numeric(dat_for_analysis$WEIGHTLBTC_A)
dat_for_analysis$HEIGHTTC_A <- as.numeric(dat_for_analysis$HEIGHTTC_A)
dat_for_analysis$AGEP_A <- as.numeric(dat_for_analysis$AGEP_A)

dat_for_analysis20[] <- lapply( dat_for_analysis20, factor)
dat_for_analysis20$WEIGHTLBTC_A <- as.numeric(dat_for_analysis20$WEIGHTLBTC_A)
dat_for_analysis20$HEIGHTTC_A <- as.numeric(dat_for_analysis20$HEIGHTTC_A)
dat_for_analysis20$AGEP_A <- as.numeric(dat_for_analysis20$AGEP_A)

#datasest split
set.seed(1846)
NN <- length(dat_for_analysis$GAD)
restrict_1 <- (runif(NN) < 0.9) 
dat_train <- subset(dat_for_analysis, restrict_1)
dat_test <- subset(dat_for_analysis, !restrict_1)

set.seed(217)
NN <- length(dat_for_analysis20$GAD)
restrict_2 <- (runif(NN) < 0.9) 
dat_train20 <- subset(dat_for_analysis20, restrict_2)
dat_test20 <- subset(dat_for_analysis20, !restrict_2)

#simple logistic 2021
model_glm1 <- glm(GAD ~ SEX + 
                    COLLEGE + HYPEV + 
                    CANEV + PREDIB + DIFF_A + 
                    COMDIFF_A1 + COMDIFF_A2 + COMDIFF_A3 + COMDIFF_A4 + 
                    COGMEMDFF_A1 + COGMEMDFF_A2 + COGMEMDFF_A3 + COGMEMDFF_A4 + 
                    NOTCOV + PAYBLL12M + CVDDIAG + RX12M +  
                    PAIFRQ3M_A1 + PAIFRQ3M_A2 + PAIFRQ3M_A3 + PAIFRQ3M_A4 +  
                    ORIENT_A1 + ORIENT_A2 + ORIENT_A3 + ORIENT_A4 + MARITAL_A1 + MARITAL_A2 + AFVET + 
                    AGEP_A, 
                  family = binomial(logit), data = dat_train)

model_glm22 <- glm(GAD ~ AGEP_A, 
                  family = binomial(logit), data = dat_for_analysis)


summary(model_glm1)
pred_vals <- predict(model_glm1, dat_test, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_test$GAD)
logistic.display(model_glm1)

df.age <- data.frame(age = seq(18,86,5))
df.age$gad <- predict(model_glm22, newdata=df.age, type="response")
  
ggplot(plot_dat, aes(x=AGEP_A, y=GAD)) + geom_line()

#simple logistic 2020
model_glm20 <- glm(GAD ~ SEX + 
                     COLLEGE + HYPEV + 
                     CANEV + PREDIB + DIFF_A + 
                     COMDIFF_A1 + COMDIFF_A2 + COMDIFF_A3 + COMDIFF_A4 + 
                     COGMEMDFF_A1 + COGMEMDFF_A2 + COGMEMDFF_A3 + COGMEMDFF_A4 + 
                     NOTCOV + PAYBLL12M + CVDDIAG + RX12M +  
                     PAIFRQ3M_A1 + PAIFRQ3M_A2 + PAIFRQ3M_A3 + PAIFRQ3M_A4 +  
                     ORIENT_A1 + ORIENT_A2 + ORIENT_A3 + ORIENT_A4 + MARITAL_A1 + MARITAL_A2 + AFVET + 
                     AGEP_A, 
                  family = binomial(logit), data = dat_train20)

summary(model_glm20)
pred_vals <- predict(model_glm20, dat_test20, type = "response")
pred_model_logit20 <- (pred_vals > 0.5)
table(pred = pred_model_logit20, true = dat_test20$GAD)
logistic.display(model_glm20)

#stepwise
model_glm <- glm(GAD ~ ., 
                  family = binomial(logit), data = dat_train)

depf_gogmem <- glm(DEPFREQ_A1 ~ COGMEMDFF_A4, 
                 family = binomial(logit), data = dat_for_analysis)

summary(model_glm)
summary(depf_gogmem)
logistic.display(model_glm)
model_glm_Stepwise <- step(model_glm, direction = "both")
summary(model_glm_Stepwise)
logistic.display(model_glm_Stepwise)

pred_vals <- predict(model_glm_Stepwise, dat_test, type = "response")
pred_model_glm_Stepwise <- (pred_vals > 0.5)
table(pred = pred_model_glm_Stepwise, true = dat_test$GAD)

#stepwise 20
model_glm20_ <- glm(GAD ~ ., 
                 family = binomial(logit), data = dat_train20)
summary(model_glm20_)
logistic.display(model_glm20_)
model_glm_Stepwise20 <- step(model_glm20_, direction = "both")
summary(model_glm_Stepwise20)
logistic.display(model_glm_Stepwise20)

pred_vals20 <- predict(model_glm_Stepwise20, dat_test20, type = "response")
pred_model_glm_Stepwise20 <- (pred_vals20 > 0.5)
table(pred = pred_model_glm_Stepwise20, true = dat_test20$GAD)