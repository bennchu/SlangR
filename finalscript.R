pacman::p_load(tidyverse,janitor,car,haven,lm.beta,lsr,multicon,psych,DescTools,phia,tidytext)
options(contrasts=c("contr.helmert",  "contr.poly")) 

transformer <- function(x){
  print("squareroot")
  squareroot <- (x+1)^.5
  print(Skew(squareroot,na.rm=TRUE, method=2,conf.level=.99))
  print("log")
  log <- log10(x+1)
  print(Skew(log,na.rm=TRUE, method=2,conf.level=.99))
  print("inverse")
  inverse <- 1/(x+1)
  print(Skew(inverse,na.rm=TRUE, method=2,conf.level=.99))
  }

unclean <- read_csv("C:/Users/Branly Mclanbry/Downloads/thesis_complete_raw.csv")
cleanslang = tbl_df(unclean) %>%
  clean_names()              %>%
  filter(progress > 95)    %>%
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
         iv_eth         =  ifelse(ethnicity == 1, "black",
                                  ifelse(ethnicity == 2, "asian",
                                         ifelse(ethnicity == 3, "asian indian",
                                                ifelse(ethnicity == 4, "pacific islander",
                                                       ifelse(ethnicity == 5, "hispanic",
                                                              ifelse(ethnicity == 6, "white", "other")))))),
         iv_age         =  as.numeric(age_4))

cleanslang <- cleanslang %>%
  mutate(
    manippers = case_when(iv_sl.cat == "known" & dv_conf > 4 ~ "hiknown",
                          iv_sl.cat == "unknown" & dv_conf < 4 ~ "lounknown"),
    dv_simstud_log = log10((max(dv_simstud) - dv_simstud) +1),
    dv_proto_sqrt  = (dv_proto+1)^.5,
    dv_hsuid_sqrt  = (dv_hsuid+1)^.5
  )

outlier <-boxplot.stats(cleanslang$duration_in_seconds)$out
boxplot(cleanslang$duration_in_seconds)
sort(outlier)

mean(cleanslang$dv_conf)
median(cleanslang$dv_conf)
tapply(cleanslang$dv_conf,cleanslang$iv_sl.cat,describe)
t.test(dv_conf~iv_sl.cat, data = cleanslang)

proto = select(unclean,protodv_protodv_1 , protodv_protodv_2 , protodv_protodv_3 , 
               protodv_protodv_4 , protodv_protodv_5)
hsuid = select(unclean,hsuiddv_hsuiddv_1 , hsuiddv_hsuiddv_2 , 
               hsuiddv_hsuiddv_3 , hsuiddv_hsuiddv_4 ,
               hsuiddv_hsuiddv_5 , hsuiddv_hsuiddv_6 , 
               hsuiddv_hsuiddv_7 , hsuiddv_hsuiddv_8 , hsuiddv_hsuiddv_9)
ent =   select(unclean,entitdv_entitdv_1 , entitdv_entitdv_2 , 
               entitdv_entitdv_3 , entitdv_entitdv_4 ,
               entitdv_entitdv_5 , entitdv_entitdv_6 , 
               entitdv_entitdv_7 , entitdv_entitdv_8 , entitdv_entitdv_9)
ost =   select(unclean,ostdv_ostdv_1 , ostdv_ostdv_2 , 
               ostdv_ostdv_3 , ostdv_ostdv_4 , 
               ostdv_ostdv_5 , ostdv_ostdv_6 , 
               ostdv_ostdv_7 , ostdv_ostdv_8 , 
               ostdv_ostdv_9 , ostdv_ostdv_10)

alpha(proto)
alpha(hsuid)
alpha(ent)
alpha(ost,check.keys =TRUE)

lapply(thesis[0:8],Skew,method = 2, conf.level =.99)   
lapply(thesis[0:8],Kurt, method = 2, conf.level = .99)

transform(thesis$dv_simstud)
transform(thesis$dv_proto)
transform(thesis$dv_hsuid)

thesis <- cleanslang %>%
  select(dv_conf,dv_simstud,dv_unslan,dv_ent,dv_proto,dv_hsuid,
         dv_unc,dv_ost,iv_uncer.cat,iv_sl.cat,iv_sl.num,iv_uncer.num,
         iv_gender,iv_class,iv_lang,iv_eth,iv_age, dv_simstud_log,
         dv_proto_sqrt,dv_hsuid_sqrt, manippers) %>%
  na.omit()

write_sav(thesis, "benthesis-postmanip.sav")

#hypothesis 1
oneway <- aov(dv_hsuid ~  + manippers,dat = thesis)
oneway <- aov(dv_hsuid_sqrt ~  + iv_uncer.cat,dat = thesis)
Anova(oneway,type = 3)
confint(oneway)
etaSquared(oneway)
cohensD(dv_hsuid ~ iv_uncer.cat,dat = thesis)
tapply(thesis.2$dv_hsuid,thesis.2$iv_uncer.cat,describe)

#hypothesis 2
oneway <- aov(dv_proto ~ manippers, dat = thesis)
oneway <- aov(dv_proto_sqrt ~ iv_sl.cat, dat = thesis)
Anova(oneway,type = 3)
etaSquared(oneway)
confint(oneway)
cohensD(dv_hsuid ~ iv_sl.cat,dat = thesis)
tapply(thesis.2$dv_hsuid,thesis.2$iv_sl.cat,describe)

#hypothesis 3
oneway <- aov(dv_ent~ manippers, dat = thesis)
Anova(oneway,type = 3)
etaSquared(oneway)
confint(oneway)
cohensD(dv_ent ~ iv_sl.cat,dat = thesis)
tapply(thesis$dv_ent,thesis$iv_sl.cat,describe)

#hypothesis 4
anovatime<-lm(dv_proto ~
                iv_age +
                iv_uncer.cat*manippers
               ,data=thesis)
anovatime.1 <- Anova(anovatime, type = 3)
anovatime.1
confint(anovatime)
etaSquared(anovatime)
mean.1 <-tapply(thesis$dv_proto,list(thesis$manippers,thesis$iv_uncer.cat),mean)
describe.1 <-tapply(thesis$dv_proto,list(thesis$manippers,thesis$iv_uncer.cat),describe)
mean.1
describe.1[1,1]
describe.1[1,2]
describe.1[2,1]
describe.1[2,2]

low <- thesis %>% filter(iv_uncer.cat == "low")
high <- thesis %>% filter(iv_uncer.cat == "high")

test.1 <- aov(dv_proto~manippers, data = low)
test.2 <- aov(dv_proto~manippers, data = high)

test.1a <- Anova(test.1, type = "III")
test.2a <- Anova(test.2, type = "III")

sswin <- anovatime.1[5,1]
dfwin <- anovatime.1[5,2]
ssse1 <- test.1a[2,1]
dfse1 <- test.1a[2,2]
ssse2 <- test.2a[2,1]
dfse2 <- test.2a[2,2]

msse1 <- (ssse1/dfse1)
mswin <- (sswin/dfwin)
msse2 <- (ssse2/dfse2)
fse1 <- (msse1/mswin)
pse1 <- 1-pf(c(fse1), df1 = dfse1, df2 = dfwin, lower.tail = TRUE)
etasqse1 <- (ssse1/(ssse1+sswin))
fse2 <- (msse2/mswin)
pse2 <- 1-pf(c(fse2), df1 = dfse2, df2 = dfwin, lower.tail = TRUE)
etasqse2 <- (ssse2/(ssse2+sswin))

fse1
pse1
etasqse1
fse2
pse2
etasqse2

#hypothesis 5
anovatime<-lm(dv_hsuid~
              iv_uncer.cat*manippers, 
              data=thesis)
Anova(anovatime, type = "III")
confint(anovatime)
etaSquared(anovatime)
mean.1 <-tapply(thesis$dv_hsuid,list(thesis$iv_sl.cat,thesis$iv_uncer.cat),mean)
describe.1 <-tapply(thesis$dv_hsuid,list(thesis$iv_sl.cat,thesis$iv_uncer.cat),describe)
mean.1
describe.1[1,1]
describe.1[1,2]
describe.1[2,1]
describe.1[2,2]

#hypothesis 6

#hypothesis 7
anovatime<-lm(dv_ent~
                iv_uncer.num*manippers
              , data=thesis)
Anova(anovatime, type = 3)
confint(anovatime)
etaSquared(anovatime)
mean.1 <-tapply(thesis$dv_ent,list(thesis$iv_sl.cat,thesis$iv_uncer.cat),mean)
yup<-tapply(thesis$dv_ent,list(thesis$iv_sl.cat,thesis$iv_uncer.cat),describe)
mean.1 
describe[1,1]
describe[1,2]
describe[2,1]
describe[2,2]