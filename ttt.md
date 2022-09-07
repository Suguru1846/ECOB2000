# Homework 2 ECO B2000

I examined the data of "Household_Pulse_data". I found some interesting facts.

## The names of the people in the study group
John Robison, Safinaz Ali


## Which genders are the most anxious?

Women are generally more anxious than men. Transgender people are most anxious, but n is very low.

```

gen_anx <- Household_Pulse_data %>%
  group_by(GENID_DESCRIBE,ANXIOUS) %>%
  summarize(n = n()) %>%
  filter(GENID_DESCRIBE != "NA" & ANXIOUS != "NA") 
male <- gen_anx[gen_anx$GENID_DESCRIBE == "male",]
male$prop <-prop.table(male$n)

female <- gen_anx[gen_anx$GENID_DESCRIBE == "female",]
female$prop <-prop.table(female$n)

trans <- gen_anx[gen_anx$GENID_DESCRIBE == "transgender",]
trans$prop <-prop.table(trans$n)

other <- gen_anx[gen_anx$GENID_DESCRIBE == "other",]
other$prop <-prop.table(other$n)

male
female
trans
other

```

## Does the Covid vaccine work?
The Covid vaccine seems to prevent Covid. The data shows that only 11.7% of vaccinated people were diagnosed with Covid, whereas 27.4% of unvaccinated received the same diagnosis--a difference of 15.7%.  In addition, 87.9% of vaccinated people did not catch Covid. On the other hand, the number fell to 70.8% for unvaccinated people--a 17.1% difference.

```
covid_vax <- Household_Pulse_data %>%
  group_by(RECVDVACC,HADCOVID) %>%
  summarize(n = n()) %>%
  filter(RECVDVACC != "NA" & HADCOVID != "NA") 

#vaccine people had Covid 17% less 
vax <- covid_vax[covid_vax$RECVDVACC == "yes got vaxx",]
vax$prop <-prop.table(vax$n)

nonvax <- covid_vax[covid_vax$RECVDVACC == "no did not get vaxx",]
nonvax$prop <-prop.table(nonvax$n)

vax
nonvax
```

#### Is education level related to the intensity of anxiety?
It does not appear that there is any relationship between education level and anxiety.

```{r education x anxius}
edu_anx <- Household_Pulse_data %>% 
  group_by(EEDUC,ANXIOUS)  %>% 
  summarize(n = n()) %>% 
  filter(EEDUC != "NA" & ANXIOUS != "NA") 

edu_anx_uni <- edu_anx %>% 
  filter(EEDUC == "adv deg" | EEDUC == "bach deg" | EEDUC == "assoc deg") 

edu_anx_uni<- 
  edu_anx_uni %>% 
  group_by(ANXIOUS)  %>% 
  summarize(n = sum(n))

edu_anx_uni$prop <-prop.table(edu_anx_uni$n)

edu_anx_high <- edu_anx %>% 
  filter(EEDUC != "adv deg" | EEDUC != "bach deg" | EEDUC != "assoc deg") 

edu_anx_high <- 
  edu_anx_high %>% 
  group_by(ANXIOUS) %>% 
  summarize(n = sum(n))

edu_anx_high$prop <-prop.table(edu_anx_high$n)

edu_anx_uni
edu_anx_high
```

#### Does anxiety make people want to get the vaccine?
According to the data,  unvaccinated people feel anxious more often than vaccinated people. People who are vaccinated and feel anxiety "nearly every day" is 13.5%, whereas unvaccinated people are at 18.8%. 
However, 33.2% of people who are vaccinated felt "several days of anxiety over past 2 wks" whereas 25.4% of unvaccinated people felt the same level of anxiety.

```{r vaccine statusx anxius}
anx_vac <- Household_Pulse_data %>%
  group_by(ANXIOUS,RECVDVACC) %>%
  summarize(n = n()) %>%
  filter(ANXIOUS != "NA" & RECVDVACC != "NA") 

#almost same result Anxious x Covid vaccine status
#vaccine people
yesvax <- anx_vac[anx_vac$RECVDVACC == "yes got vaxx",]
yesvax$prop <-prop.table(yesvax$n)

#Non vaccine people
novax <- anx_vac[anx_vac$RECVDVACC == "no did not get vaxx",]
novax$prop <-prop.table(novax$n)

yesvax
novax
```

#### Race ratio
Caucasians are a large part of the survey.

```{r race}
summary(Household_Pulse_data$RRACE)
prop.table(summary(Household_Pulse_data$RRACE))
```
