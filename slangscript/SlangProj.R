library(tidyverse)
char_data<- read.csv("C:/Users/Branly Mclanbry/Downloads/Humboldt+Slangiversity_August+30%2C+2017_18.07/Humboldt Slangiversity_August 30, 2017_18.07.csv", stringsAsFactors=FALSE)
num_data <- data.frame(data.matrix(char_data))


numeric_columns <- sapply(num_data,function(x){mean(as.numeric(is.na(x)))<0.5})
unclean <- data.frame(num_data[,numeric_columns], char_data[,!numeric_columns])



cleanslang = tbl_df(unclean)[-c(1,2),-c(1,3:6,65:75)] %>%
  filter(Progress == 100) %>%
  mutate(confavg = (slangconf1 + slangconf2)/2, 
         simstudentavg = simstudent_1,
         uncertslangavg = uncertslang_1)

#mutate(confavg = mean(sum(c(slangconf1, slangconf2)))

mean(sum(c(grepl(slangconf[1-9])))
     