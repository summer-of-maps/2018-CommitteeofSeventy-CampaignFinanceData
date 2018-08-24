install.packages("dplyr")
library(dplyr)


#this script combines datasets 



#campaign finance data 
dat_finance <- read.csv("../campaign_finance_proj/output/campaign_finance_by_ward.csv")

#census data 
dat_cen <- read.csv('../census_data_proj/output/census_var_by_ward.csv')

#voter turnout and election results: 
dat_vt_er <- read.csv('../voter_turnout_and_election_results_proj/output/vot_turnout_and_cand_pvote.csv')

#join 
#join 
dat_join <- full_join(dat_finance, dat_cen, by = "ward")
dat_join <- full_join(dat_join, dat_vt_er, by = "ward")

write.csv(dat_join,"output/data_by_ward.csv" )
