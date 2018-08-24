install.packages("dplyr")
library(dplyr)


# this script creates summary tables created with de-dupped data 
# and filters for only Democrat-affiliated CFR records 

### filter out Republican data since analysis is on Democratic Primary DA election 2017----------------

# read in campaign finance data with duplicates indicated, filter for most recent amendment 
  dat <- read.csv("output/dat_clean_with_dups_indicated.csv")
  dat <- dat %>% filter(intention == "keep")

# read in ward filers and entities with their Party noted # this is the file that Claire Michel, a fellow at C70 created for us
  dat_party <- read.csv("raw_data/campaign_finance_ward_dem_rep_noted.csv.csv")

  dat_party_f <- dat_party %>% filter(category == "filer")
  colnames(dat_party_f)[which(names(dat_party_f) == "name")] <- "FilerName"
  colnames(dat_party_f)[which(names(dat_party_f) == "party")] <- "party.f"
  
  dat_party_e <- dat_party %>% filter(category == "entity")
  colnames(dat_party_e)[which(names(dat_party_e) == "name")] <- "EntityName" 
  colnames(dat_party_e)[which(names(dat_party_e) == "party")] <- "party.e" 
  
# join both by entity name and by filer name 
  dat_join <- merge(dat,dat_party_f, by = "FilerName", all = TRUE)
  dat_join <- merge(dat_join, dat_party_e, by = "EntityName", all = TRUE)
  
# indicate D or R
  dat_join <- dat_join %>% mutate(poli_party = case_when(party.f == "Dem" | party.e == "Dem" ~ "D", 
                                                         party.f == "Rep" | party.e == "Rep" ~ "R"))
# clean up the columns 
  dat_clean <- subset(dat_join, select = -c(category.x, party.f, X.y, notes.x, category.y, party.e, notes.y))
  write.csv(dat_clean, "output/campaign_finance_dups_party_indicated.csv") 
  
# filter for democrat party 
  dat_clean <- dat_clean %>% filter(poli_party == "D")

# table where each ward is a row to continually join to: 
  d <- read.csv("raw_data/wards.csv")
  d$ward <- as.character(d$ward)
  
  
# create summary tables --------------
  ## CREATING SUMMARY TABLES 
# --> money candidates spent on each ward 
cand_spent_wards <- dat_clean %>% filter(grepl("Schedule III", DocType))                                                                                    
cand_spent_wards <- cand_spent_wards %>% mutate_all(funs(replace(., is.na(.), 0)))%>% group_by(cand_filer, ward_ent) %>% summarise(cand_spent_wards = sum(Amount)) %>% as.data.frame()

write.csv(cand_spent_wards, 'output/cand_spent_wards.csv', row.names = FALSE) 
    cand_spent_wards$ward <- cand_spent_wards$ward_ent
    cand_spent_wards$ward <- as.character(cand_spent_wards$ward)
    cand_spent_wards <- cand_spent_wards %>% select(cand_filer, cand_spent_wards, ward)
    cand_sp_w_total <- cand_spent_wards %>% group_by(ward) %>% summarise(c_sp_w_total = sum(cand_spent_wards)) %>% as.data.frame()
# candidate spent on wards WIDE data 
    csw <- cand_spent_wards
    csw$csw <- csw$cand_spent_wards
    csw <- csw %>% select(csw, cand_filer, ward)
    c_s_wards <- reshape(csw, idvar = "ward", timevar = "cand_filer", direction = "wide")
    c_s_wards$csw_lk <- c_s_wards$csw.larry
    c_s_wards$csw_na <-c_s_wards$csw.NA
    c_s_wards$csw_negrin <- c_s_wards$csw.negrin
    c_s_wards$csw_tariq <-c_s_wards$csw.tariq 
    c_s_wards$csw_teresa <-c_s_wards$csw.teresa
    c_s_wards$csw_untermeyer <-c_s_wards$csw.untermeyer
    c_s_wards <- c_s_wards %>% subset(select = -c(csw.larry, csw.negrin, csw.tariq, csw.teresa, csw.untermeyer, csw.NA))
    c_s_wards <- c_s_wards %>% mutate_all(funs(replace(., is.na(.), 0)))%>% as.data.frame()
    
    #join 
    dat_money <- full_join(d, c_s_wards, by = "ward")
    
    
# --> money candidates received
cand_rec <- dat_clean %>% filter(grepl("Schedule I ", DocType)) #remember the space after Schedule I
unique(dat_clean$cand_filer)
unique(cand_rec$cand_filer)
cand_rec <- cand_rec %>% mutate_all(funs(replace(., is.na(.), 0)))%>% group_by(cand_filer) %>% summarise(cand_rec_total = sum(Amount)) %>% as.data.frame()
write.csv(cand_rec, 'output/cand_rec.csv', row.names = FALSE) 

    ## note this one very interesting -- only untermeyer is showing up. I checked dat_clean: larry, tariq, teresa, and untermeyer and negrin show up as cand_filers 


# --> all money wards spent 
ward_spent <- dat_clean %>% filter(grepl("Schedule III", DocType))
ward_spent <- ward_spent %>% mutate_all(funs(replace(., is.na(.), 0)))%>% group_by(ward_filer) %>% summarise(ward_spent_total = sum(Amount)) %>% as.data.frame()
write.csv(ward_spent, 'output/ward_spent.csv', row.names = FALSE)
  ward_spent$ward <- as.character(ward_spent$ward_filer)
  ward_spent$w_sp_total <- ward_spent$ward_spent_total
  w_sp_total <- ward_spent %>% select(ward, w_sp_total)
  
  #join 
  dat_money <- full_join(dat_money, w_sp_total, by = "ward")


# --> money received by wards 
ward_rec_total <- dat_clean %>% filter(grepl("Schedule I ", DocType))  #remember the space after Schedule I
ward_rec_total <- ward_rec_total %>% mutate_all(funs(replace(., is.na(.), 0)))%>% group_by(ward_filer) %>% summarise(ward_rec_total = sum(Amount))%>% as.data.frame()
ward_rec_total <- ward_rec_total %>% filter(ward_filer != 0) #take away row for Schedule 1 money that wasn't received by a ward (Used to be NA but is now 0)
write.csv(ward_rec_total, 'output/ward_rec_total.csv', row.names = FALSE) 
  ward_rec_total$ward <- as.character(ward_rec_total$ward_filer)
  ward_rec_total$w_r_total <- ward_rec_total$ward_rec_total
  w_r_total <- ward_rec_total %>% select(ward, w_r_total)
  
  #join
  dat_money <- full_join(dat_money, w_r_total, by = "ward")

# --> money received by wards, not from candidates (external = not from candidates)
ward_rec_external <- dat_clean  %>% filter(grepl("Schedule I ", DocType))   #remember the space after Schedule I
ward_rec_external <- ward_rec_external %>% filter(is.na(cand_ent))
ward_rec_external <- ward_rec_external %>% mutate_all(funs(replace(., is.na(.), 0)))%>% group_by(ward_filer) %>% summarise(ward_rec_external = sum(Amount)) %>% as.data.frame()
# this data is also in the first section of the cand_spent_wards where cand_ENT is 0 
write.csv(ward_rec_external, 'output/ward_rec_not_from_cand.csv', row.names = FALSE)

  w_r_notcand <- ward_rec_external
  w_r_notcand$ward <- as.character(w_r_notcand$ward_filer)
  w_r_notcand$w_r_notcand <- w_r_notcand$ward_rec_external
  w_r_notcand <- w_r_notcand %>% select(ward, w_r_notcand)
  
  #join
  dat_money <- full_join(dat_money, w_r_notcand, by = "ward")

# --> money received by wards from candidates 
ward_rec_cand <- dat_clean %>% filter(grepl("Schedule I ", DocType))  #remember the space after Schedule I
ward_rec_cand <- ward_rec_cand %>% mutate_all(funs(replace(., is.na(.), 0)))%>% group_by(ward_filer, cand_ent) %>% summarise(total_amt = sum(Amount))%>% as.data.frame()
ward_rec_cand <- ward_rec_cand %>% filter(!cand_ent == 0) #take away wards that didn't receive money from candidates 
ward_rec_cand <- ward_rec_cand %>% group_by(ward_filer) %>% summarise(total_amt = sum(total_amt)) %>% as.data.frame()# if you keep cand_ent in the group_by you could make a really cool stacked bar graph
ward_rec_cand <- ward_rec_cand %>% filter(!ward_filer == 0) 
write.csv(ward_rec_cand, 'output/ward_rec_cand.csv', row.names = FALSE)
  ward_rec_cand$ward <- as.character(ward_rec_cand$ward_filer)
  ward_rec_cand$w_r_cand <- ward_rec_cand$total_amt
  w_r_cand <- ward_rec_cand %>% select(ward, w_r_cand)

  #join
  dat_money <- full_join(dat_money, w_r_cand, by = "ward")
  

# --> money wards spent on GOTV 
ward_spent_gotv <- dat_clean%>% filter(grepl("gotv|get out the vote|g.o.t.v|g o t v|get out the vote ", Description, ignore.case = TRUE))
ward_spent_gotv <- ward_spent_gotv %>% mutate_all(funs(replace(., is.na(.), 0)))%>% group_by(ward_filer) %>% summarise(ward_spent_gotv = sum(Amount))%>% as.data.frame()
write.csv(ward_spent_gotv, 'output/ward_spent_gotv.csv', row.names = FALSE)
  ward_spent_gotv$ward <- as.character(ward_spent_gotv$ward_filer)
  ward_spent_gotv$w_s_gotv <- ward_spent_gotv$ward_spent_gotv
    w_s_gotv <- ward_spent_gotv %>% select(ward, w_s_gotv) 

    #join
    dat_money <- full_join(dat_money, w_s_gotv, by = "ward")
    
#final campaign finance data (corrected August 19th)
write.csv(dat_money, "output/campaign_finance_by_ward.csv") 



