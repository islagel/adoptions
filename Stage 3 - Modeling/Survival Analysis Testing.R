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


adop.1.1=adoptions%>%
  filter(summer==1, chip_status==1)
adop.0.1=adoptions%>%
  filter(summer==0, chip_status==1)
adop.1.0=adoptions%>%
  filter(summer==1, chip_status==0)
adop.0.0=adoptions%>%
  filter(summer==0, chip_status==0)

surv.mod.1.1=coxph(Surv(days_in_shelter, censored)~pitbull,
               data=adop.1.1)
surv.mod.0.1=coxph(Surv(days_in_shelter, censored)~pitbull,
                   data=adop.0.1)
surv.mod.1.0=coxph(Surv(days_in_shelter, censored)~pitbull,
                   data=adop.1.0)
surv.mod.0.0=coxph(Surv(days_in_shelter, censored)~pitbull,
                   data=adop.0.0)

surv.mod.1.1
surv.mod.1.0
surv.mod.0.1
surv.mod.0.0

(surv.mod.1.1$coefficients+surv.mod.1.0$coefficients+surv.mod.0.1$coefficients+surv.mod.0.0$coefficients)/4

surv.mod=coxph(Surv(days_in_shelter, censored)~pitbull+summer+pitbull:summer, adoptions)


kp_curve.1.1=survfit(Surv(days_in_shelter, censored)~pitbull,
      data=adop.1.1)
kp_curve.1.0=survfit(Surv(days_in_shelter, censored)~pitbull,
                     data=adop.1.0)
kp_curve.0.1=survfit(Surv(days_in_shelter, censored)~pitbull,
                     data=adop.0.1)
kp_curve.0.0=survfit(Surv(days_in_shelter, censored)~pitbull,
                     data=adop.0.0)

kp_plot.1.1=ggsurvplot(kp_curve.1.1, data=adoptions)+
  ggtitle("summer=1, chip=1")
kp_plot.1.0=ggsurvplot(kp_curve.1.0, data=adoptions)+
  ggtitle("summer=1, chip=0")
kp_plot.0.1=ggsurvplot(kp_curve.0.1, data=adoptions)+
  ggtitle("summer=0, chip=1")
kp_plot.0.0=ggsurvplot(kp_curve.0.0, data=adoptions)+
  ggtitle("summer=0, chip=0")

kp_plot.0.0
kp_plot.0.1
kp_plot.1.0
kp_plot.1.1

library(gridExtra)

