library(tidyverse)
library(janitor)

#for when I'm at school
unclean <- read_csv("U:/rawslangdat.csv")
#for when I'm at home
unclean<- read.csv("C:/Users/Branly Mclanbry/Downloads/rawslangdat.csv")
cleanslang = tbl_df(unclean) %>%
  clean_names() %>%
  filter(progress == 100)%>%
  mutate(conf_avg    = (slangconf1 + slangconf2)/2,
         simstud_avg = (simstudent_1),
         unslan_avg  = (uncertslang_1),
         ent_avg     = (entitdv_entitdv_1 + entitdv_entitdv_2 + entitdv_entitdv_3 + entitdv_entitdv_4 + 
                        entitdv_entitdv_5 + entitdv_entitdv_6 + entitdv_entitdv_7 + entitdv_entitdv_8 + 
                        entitdv_entitdv_9)/9,
         proto_avg   = (protodv_protodv_1 + protodv_protodv_2 + protodv_protodv_3 + protodv_protodv_4 + 
                        protodv_protodv_5)/5,
         hsuid_avg   = (hsuiddv_hsuiddv_1 + hsuiddv_hsuiddv_2 + hsuiddv_hsuiddv_3 + hsuiddv_hsuiddv_4 +
                        hsuiddv_hsuiddv_5 + hsuiddv_hsuiddv_6 + hsuiddv_hsuiddv_7 + hsuiddv_hsuiddv_8 +
                        hsuiddv_hsuiddv_9)/9,
         unc_avg     = (uncdv_uncdv_1 + uncdv_uncdv_2 + uncdv_uncdv_3 + uncdv_uncdv_4 +
                        uncdv_uncdv_5)/5,
         ost_avg     = (ostdv_ostdv_1 + ostdv_ostdv_2 + ostdv_ostdv_3 + ostdv_ostdv_4 + ostdv_ostdv_5 +
                        ostdv_ostdv_6 + ostdv_ostdv_7 + ostdv_ostdv_8 + ostdv_ostdv_9 +
                        ostdv_ostdv_10)/10) 
     
         
         
names(cleanslang)

test1.1$uncertcat<-recode(test1.1$hiunc_1,"NA='low';else='high'")
test1.1$slangcat<-recode(test1.1$knownmanipcheck, "NA='unknown';else='known'")
