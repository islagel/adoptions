library(survival)
library(survminer)
library(tidyverse)
library(flexsurv)
library(readr)
library(lubridate)
adoptions <- read_csv("adoptions.csv")


adoptions=adoptions%>%
  mutate(intake_date=as.Date(intake_date),
         outcome_date=as.Date(outcome_date))%>%
  mutate(days_in_shelter=difftime(outcome_date, intake_date, units = "days"),
         days_in_shelter=as.numeric(days_in_shelter)+1)%>%
  filter(outcome_type!="DEAD ON ARRIVAL")%>%
  mutate(death=ifelse(outcome_type=="DIED", 1, 0),
         death=ifelse(outcome_type=="EUTHANIZED", 1, death), 
         censored=ifelse(death==1, 2, 1))%>%
  filter(animal_type=="DOG")%>%
  mutate(animal_breed=as.factor(animal_breed))%>%
  mutate(healthy_intake=ifelse(grepl("^HEALTHY.*",intake_condition),1,0),
         contagious_intake=ifelse(grepl(".*[^NON-]CONTAGIOUS", intake_condition),1,0),
         untreatable_intake=ifelse(grepl(".*(UNTREATABLE).*", intake_condition),1,0),
         treatable_intake=ifelse(grepl("^TREATABLE.*", intake_condition),1,0),
         manageable_intake=ifelse(grepl(".*MANAGEABLE.*", intake_condition),1,0),
         rehabitable_intake=ifelse(grepl(".*REHABILITABLE.*", intake_condition),1,0),
         normal_intake=ifelse(grepl(".*NORMAL.*", intake_condition),1,0),
         chip_status = ifelse(chip_status=="SCAN CHIP", 1, 0),
         summer = ifelse(month %in% c(5, 6, 7, 8, 9), 1, 0))

names(adoptions)
surv.mod=coxph(Surv(days_in_shelter, censored)~pitbull+strata(chip_status, summer, contagious_intake),
               data=adoptions)
summary(surv.mod)


kp_curve=survfit(Surv(days_in_shelter, censored)~pitbull+strata(chip_status, summer, contagious_intake),
      data=adoptions)
kp_curve
ggsurvplot(kp_curve, data=adoptions)

test.ph <- cox.zph(surv.mod)
test.ph


adoptions$days_in_shelter

# Ok so the proportional hazards condition is not met. 

adoptions%>%
  ggplot(aes(x=days_in_shelter))+
  geom_density()
min(adoptions$days_in_shelter, na.rm = T)

param.surv=flexsurvreg(Surv(time=days_in_shelter, event=censored)~chip_status, data=adoptions, dist="exp")
survival::



