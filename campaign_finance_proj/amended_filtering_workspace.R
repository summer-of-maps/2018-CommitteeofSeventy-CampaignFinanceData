# this script is to play around with amended reports 

install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)
install.packages("data.table")
library(data.table)


#script description 
# this script takes in the campaign finance data that was cleaned for 
# cycle/year and has columns denoting wardfiler, candfiler, wardent, candent 
# and runs a for loop to denote "keep" or "dup" for rows based on submission date 


dat <- read.csv('output/campaign_finance_clean.csv')
dat_ <- dat

dat_$SubDate <- as.Date(dat_$SubDate, format = "%m/%d/%Y")


dat_ <- dat_ %>% mutate(concat = paste(FilerName, Year, Cycle, DocType, sep = ".", collapse = NULL))

#make a new column in dat_ to populate in the for loop: 
dat_$intention <- "blank"

unique_concat <- as.vector(unique(dat_$concat))

dat_cc <- data.frame()

## this for loop takes ~12 minutes or so
for(i in seq_along(unique_concat)){
  
  this_concat <- unique_concat[i]
  
  recent <- dat_ %>% filter(concat == this_concat) %>% 
    slice(which.max(SubDate))
  
  recent <- recent$SubDate
 
  dat_dupped <- dat_ %>% filter(concat == this_concat) %>% mutate(intention = if_else(SubDate == recent, "keep", "dups")) 
  
  assign(paste0("data_", this_concat), dat_dupped)
  dat_cc <- rbind(dat_cc, dat_dupped)           
} 
write.csv(dat_cc, "output/dat_clean_with_dups_indicated.csv", row.names = FALSE)



