#####
library(tidyverse)
library(janitor)
library(car)
library(foreign)
library(lm.beta)
library(lsr)
options(contrasts=c("contr.helmert",  "contr.poly")) 
#####
unclean <-  read_csv("C:/Users/bc1256/Desktop/Humboldt Slangiversity_October 25, 2017_12.23.csv")
unclean <- read_csv("C:/Users/Branly Mclanbry/Downloads/Humboldt+Slangiversity_October+25%2C+2017_22.45/Humboldt Slangiversity_October 25, 2017_22.45.csv")
cleanslang = tbl_df(unclean) %>%
  clean_names()              %>%
  filter(progress == 100)    %>%
  mutate(dv_conf    = (slangconf1 + slangconf2)/2,
         dv_simstud = (simstudent_1),
         dv_unslan  = (uncertslang_1),
         dv_ent     = (entitdv_entitdv_1 + entitdv_entitdv_2 + entitdv_entitdv_3 + entitdv_entitdv_4 + 
                         entitdv_entitdv_5 + entitdv_entitdv_6 + entitdv_entitdv_7 + entitdv_entitdv_8 + 
                         entitdv_entitdv_9)/9,
         dv_proto   = (protodv_protodv_1 + protodv_protodv_2 + protodv_protodv_3 + protodv_protodv_4 + 
                         protodv_protodv_5)/5,
         dv_hsuid   = (hsuiddv_hsuiddv_1 + hsuiddv_hsuiddv_2 + hsuiddv_hsuiddv_3 + hsuiddv_hsuiddv_4 +
                         hsuiddv_hsuiddv_5 + hsuiddv_hsuiddv_6 + hsuiddv_hsuiddv_7 + hsuiddv_hsuiddv_8 +
                         hsuiddv_hsuiddv_9)/9,
         dv_unc     = (uncdv_uncdv_1 + uncdv_uncdv_2 + uncdv_uncdv_3 + uncdv_uncdv_4 +
                         uncdv_uncdv_5)/5,
         dv_ost     = (ostdv_ostdv_1 + ostdv_ostdv_2 + ostdv_ostdv_3 + ostdv_ostdv_4 + ostdv_ostdv_5 +
                         ostdv_ostdv_6 + ostdv_ostdv_7 + ostdv_ostdv_8 + ostdv_ostdv_9 +
                         ostdv_ostdv_10)/10,
         iv_uncer.cat   =  ifelse(is.na(hiunc_1),"low","high"),
         iv_uncer.num   =  ifelse(is.na(hiunc_1),0,1),
         iv_sl.cat      =  ifelse(is.na(knownmanipcheck),"unknown","known"),
         iv_sl.num      =  ifelse(is.na(knownmanipcheck),0,1),
         iv_gender      =  ifelse(gender == 1, "male", 
                                  ifelse(gender == 2,"female", "other")),
         iv_class       =  ifelse(classstand == 1, "freshman", 
                                  ifelse(classstand == 2, "sophmore",
                                         ifelse(classstand == 3, "junior",
                                                ifelse(classstand == 4, "senior","graduate")))),
         iv_lang        =  ifelse(nation == 1, "English as first language", "other"),
         iv_eth         =  ifelse(nation == 1, "black",
                                  ifelse(nation == 2, "asian",
                                         ifelse(nation == 3, "asian indian",
                                                ifelse(nation == 4, "pacific islander",
                                                       ifelse(nation == 5, "hispanic",
                                                              ifelse(nation == 6, "white", "other")))))),
         iv_age         =  as.numeric(age_4))

unclean <-  read_csv("C:/Users/bc1256/Desktop/Humboldt Slangiversity_October 25, 2017_12.23.csv")

cleanslang = (unclean) %>%
  clean_names()              %>%
  filter(progress == 100)    %>%
  mutate(dv_conf    = (slangconf1 + slangconf2)/2,
         dv_simstud = (simstudent_1),
         dv_unslan  = (uncertslang_1),
         dv_ent     = (entitdv_entitdv_1 + entitdv_entitdv_2 + entitdv_entitdv_3 + entitdv_entitdv_4 + 
                         entitdv_entitdv_5 + entitdv_entitdv_6 + entitdv_entitdv_7 + entitdv_entitdv_8 + 
                         entitdv_entitdv_9)/9,
         dv_proto   = (protodv_protodv_1 + protodv_protodv_2 + protodv_protodv_3 + protodv_protodv_4 + 
                         protodv_protodv_5)/5,
         dv_hsuid   = (hsuiddv_hsuiddv_1 + hsuiddv_hsuiddv_2 + hsuiddv_hsuiddv_3 + hsuiddv_hsuiddv_4 +
                         hsuiddv_hsuiddv_5 + hsuiddv_hsuiddv_6 + hsuiddv_hsuiddv_7 + hsuiddv_hsuiddv_8 +
                         hsuiddv_hsuiddv_9)/9,
         dv_unc     = (uncdv_uncdv_1 + uncdv_uncdv_2 + uncdv_uncdv_3 + uncdv_uncdv_4 +
                         uncdv_uncdv_5)/5,
         dv_ost     = (ostdv_ostdv_1 + ostdv_ostdv_2 + ostdv_ostdv_3 + ostdv_ostdv_4 + ostdv_ostdv_5 +
                         ostdv_ostdv_6 + ostdv_ostdv_7 + ostdv_ostdv_8 + ostdv_ostdv_9 +
                         ostdv_ostdv_10)/10,
         iv_uncer.num   =  ifelse(is.na(hiunc_1),0,1),
         iv_sl.num      =  ifelse(is.na(knownmanipcheck),0,1),
         iv_gender      =  gender,
         iv_class       =  classstand,
         iv_lang        =  nation,
         iv_eth         =  ethnicity,
         iv_age         =  as.numeric(age_4))
  
  
library(haven)
write_sav(cleanslang, "testgod1.sav")

#this is for cause i'm too lazy to keep looking cleanslang
thesis = cleanslang %>%
  select(dv_conf,dv_simstud,dv_unslan,dv_ent,dv_proto,dv_hsuid,dv_unc,dv_ost,
         iv_uncer.cat,iv_uncer.num,iv_sl.cat,iv_sl.num,
         iv_gender,iv_class,iv_lang,iv_eth,iv_age)

ben.thesis = cleanslang %>%
  select(dv_conf,dv_simstud,dv_unslan,dv_ent,dv_proto,dv_hsuid,dv_unc,dv_ost,
        iv_uncer.num,iv_sl.num,
         iv_age)

write_sav(ben.thesis, "cleanben.sav")

thesis.corrplot   = cor(thesis.corr)
thesis.corrplot.p = cor.mtest(thesis, conf.level = .95)
library(corrplot)
corrplot(thesis.corrplot, method = "number")


#####Analysis

cont.conf <- aov(dv_simstud ~ iv_uncer.cat + dv_conf, data = thesis)
Anova(cont.conf,type = 3)


#slang and age
cont.conf <- aov(dv_simstud ~ iv_sl.cat + iv_age, data = thesis)
Anova(cont.conf,type = 3)

cont.conf <- aov(dv_unslan ~ iv_sl.cat + iv_age, data = thesis)
Anova(cont.conf,type = 3)

cont.conf <- aov(dv_ent ~ iv_sl.cat + iv_age, data = thesis)
Anova(cont.conf,type = 3)

cont.conf <- aov(dv_hsuid ~ iv_sl.cat + iv_age, data = thesis)
Anova(cont.conf,type = 3)

cont.conf <- aov(dv_proto ~ iv_sl.cat + iv_age, data = thesis)
Anova(cont.conf,type = 3)

cont.conf <- aov(dv_unc ~ iv_sl.cat + iv_age, data = thesis)
Anova(cont.conf,type = 3)

cont.conf <- aov(dv_ost ~ iv_sl.cat + iv_age, data = thesis)
Anova(cont.conf,type = 3)
#####
#similarity to students
anovatime<-lm(dv_simstud~
                iv_uncer.cat+
                iv_sl.cat+
                dv_conf+
                iv_uncer.cat:iv_sl.cat+
                iv_uncer.cat:dv_conf+
                iv_sl.cat:dv_conf+
                iv_sl.cat:iv_uncer.cat:dv_conf
              , data=thesis)
Anova(anovatime, type = 3)
anovatime<-lm(dv_simstud ~
                dv_conf +
                iv_uncer.cat + 
                iv_sl.cat + 
                iv_uncer.cat:iv_sl.cat , data=thesis)
Anova(anovatime, type = 3)

oneway<-aov(dv_simstud~iv_uncer.cat, data=thesis)
summary(oneway)
otherway<-aov(dv_simstud~iv_sl.cat,data=thesis)
summary(otherway)



#slang uncertainty
anovatime<-lm(dv_unslan~iv_uncer.cat+
                iv_sl.cat+
                dv_conf+
                iv_uncer.cat:iv_sl.cat+
                iv_uncer.cat:dv_conf+
                iv_sl.cat:dv_conf+
                iv_sl.cat:iv_uncer.cat:dv_conf, data=thesis)
Anova(anovatime, type = 3)
anovatime<-lm(dv_unslan~
                iv_uncer.cat+
                iv_sl.cat + 
                iv_uncer.cat:iv_sl.cat
              , data=thesis)
Anova(anovatime, type = 3)
oneway<-aov(dv_unslan~iv_uncer.cat, data=thesis)
summary(oneway)
otherway<-aov(dv_unslan~iv_sl.cat,data=thesis)
summary(otherway)

#entitativity
anovatime<-lm(dv_ent~iv_uncer.cat+
                iv_sl.cat+
                dv_conf+
                iv_uncer.cat:iv_sl.cat+
                iv_uncer.cat:dv_conf+
                iv_sl.cat:dv_conf+
                iv_sl.cat:iv_uncer.cat:dv_conf, data=thesis)
Anova(anovatime, type = 3)
anovatime<-lm(dv_ent~
                iv_uncer.cat+
                iv_sl.cat + 
                iv_uncer.cat:iv_sl.cat
              , data=thesis)
Anova(anovatime, type = 3)
oneway<-aov(dv_ent~iv_uncer.cat, data=thesis)
summary(oneway)
otherway<-aov(dv_ent~iv_sl.cat,data=thesis)
summary(otherway)

#prototypicality
anovatime<-lm(dv_proto~iv_uncer.cat+
                iv_sl.cat+
                dv_conf+
                iv_uncer.cat:iv_sl.cat+
                iv_uncer.cat:dv_conf+
                iv_sl.cat:dv_conf+
                iv_sl.cat:iv_uncer.cat:dv_conf, data=thesis)
Anova(anovatime, type = 3)
anovatime<-lm(dv_proto~
                iv_uncer.cat+
                iv_sl.cat + 
                iv_uncer.cat:iv_sl.cat
              , data=thesis)
Anova(anovatime, type = 3)
oneway<-aov(dv_proto~iv_uncer.cat, data=thesis)
summary(oneway)
otherway<-aov(dv_proto~iv_sl.cat,data=thesis)
summary(otherway)

#identification as HSU
anovatime<-lm(dv_hsuid~iv_uncer.cat+
                iv_sl.cat+
                dv_conf+
                iv_uncer.cat:iv_sl.cat+
                iv_uncer.cat:dv_conf+
                iv_sl.cat:dv_conf+
                iv_sl.cat:iv_uncer.cat:dv_conf, data=thesis)
Anova(anovatime, type = 3)
anovatime<-lm(dv_hsuid~
                iv_uncer.cat+
                iv_sl.cat + 
                iv_uncer.cat:iv_sl.cat
              , data=thesis)
Anova(anovatime, type = 3)
oneway<-aov(dv_hsuid~iv_uncer.cat, data=thesis)
summary(oneway)
otherway<-aov(dv_hsuid~iv_sl.cat,data=thesis)
summary(otherway)

#Overall uncertainty
anovatime<-lm(dv_unc~iv_uncer.cat+
                iv_sl.cat+
                dv_conf+
                iv_uncer.cat:iv_sl.cat+
                iv_uncer.cat:dv_conf+
                iv_sl.cat:dv_conf+
                iv_sl.cat:iv_uncer.cat:dv_conf, data=thesis)
Anova(anovatime, type = 3)
anovatime<-lm(dv_unc~
                iv_uncer.cat+
                iv_sl.cat + 
                iv_uncer.cat:iv_sl.cat
              , data=thesis)
Anova(anovatime, type = 3)
oneway<-aov(dv_unc~iv_uncer.cat, data=thesis)
summary(oneway)
otherway<-aov(dv_unc~iv_sl.cat,data=thesis)
summary(otherway)

#ostracism 
anovatime<-lm(dv_ost~iv_uncer.cat+
                iv_sl.cat+
                dv_conf+
                iv_uncer.cat:iv_sl.cat+
                iv_uncer.cat:dv_conf+
                iv_sl.cat:dv_conf+
                iv_sl.cat:iv_uncer.cat:dv_conf, data=thesis)
Anova(anovatime, type = 3)
anovatime<-lm(dv_ost~
                iv_uncer.cat+
                iv_sl.cat + 
                iv_uncer.cat:iv_sl.cat
              , data=thesis)
Anova(anovatime, type = 3)
oneway<-aov(dv_ost~iv_uncer.cat, data=thesis)
summary(oneway)
otherway<-aov(dv_ost~iv_sl.cat,data=thesis)
summary(otherway)


#similarity to students
anovatime<-lm(dv_simstud ~
                dv_conf +
                iv_uncer.cat + 
                iv_sl.cat + 
                iv_uncer.cat:iv_sl.cat, data=thesis)
Anova(anovatime, type = 3)
tapply(thesis$dv_simstud, list(thesis$iv_uncer.cat,thesis$iv_sl.cat),mean)
#slang uncertainty
anovatime<-lm(dv_unslan~
                iv_uncer.cat + 
                iv_sl.cat + 
                iv_uncer.cat:iv_sl.cat +
                dv_conf, data=thesis)
Anova(anovatime, type = 3)
tapply(thesis$dv_unslan, list(thesis$iv_uncer.cat,thesis$iv_sl.cat),mean)
#entitativity
anovatime<-lm(dv_ent~
                dv_conf +
                iv_uncer.cat + 
                iv_sl.cat + iv_uncer.cat:iv_sl.cat , data=thesis)
Anova(anovatime, type = 3)
tapply(thesis$dv_ent, list(thesis$iv_uncer.cat,thesis$iv_sl.cat),mean)
#prototypicality
anovatime<-lm(dv_proto~
                dv_conf +
                iv_uncer.cat + 
                iv_sl.cat + 
                iv_uncer.cat:iv_sl.cat , data=thesis)
Anova(anovatime, type = 3)
tapply(thesis$dv_proto, list(thesis$iv_uncer.cat,thesis$iv_sl.cat),mean)
#identification as HSU
anovatime<-lm(dv_hsuid~
              dv_conf +
                iv_uncer.cat + 
                iv_sl.cat + 
                iv_uncer.cat:iv_sl.cat , data=thesis)
Anova(anovatime, type = 3)
tapply(thesis$dv_hsuid, list(thesis$iv_uncer.cat,thesis$iv_sl.cat),mean)
#Overall uncertainty
anovatime<-lm(dv_unc~
                dv_conf +
                iv_uncer.cat + 
                iv_sl.cat + 
                iv_uncer.cat:iv_sl.cat , data=thesis)
Anova(anovatime, type = 3)
tapply(thesis$dv_unc, list(thesis$iv_uncer.cat,thesis$iv_sl.cat),mean)

#ostracism 
anovatime<-lm(dv_ost~iv_uncer.cat:iv_sl.cat:dv_conf , data=thesis)
Anova(anovatime, type = 3)
summary(anovatime)
lm.beta(anovatime)
#similarity to students
lm.beta(anovatime)
anovatime<-lm(dv_simstud~iv_uncer.cat:iv_sl.cat:dv_conf , data=thesis)
Anova(anovatime, type = 3)
summary(anovatime)
lm.beta(anovatime)
