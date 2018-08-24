#install.packages("dplyr")
install.packages("stringr")
library(stringr) #used to get a 
library(dplyr)

# this script takes in election results from 2017, and registered voter data for 2017 
# voter turnout is calculated by dividing votes for Democratic DA office by registered democratic voters, per ward 
# candidate's percentage of the votes is calculated by dividing votes for a candidate by total votes cast, per ward 
# the output is voter turnout by ward in 2017 as a percentage, and percentage of the vote won by candidates 



### election results: 2017_primary_csv (NUMERATOR) ---------------
dat_er <- read.csv("raw_data/2017_primary.csv")
dat_er$ward_str <- as.character(dat_er$WARD)

dat_er_dem <- dat_er %>% filter(OFFICE == "DISTRICT ATTORNEY-DEM") # I'm going to only use Democratic Votes 

#if you wanted turnout for both parties you could change the filter 
#dat_er_both <- dat_er %>% filter(grepl("DISTRICT ATTORNEY-DEM|DISTRICT ATTORNEY-REP", OFFICE, ignore.case = FALSE))

#allocate divisions to wards that are split into A and B 
d <- dat_er_dem %>% mutate(ward_str = ifelse(DIVISION <= 24 & ward_str == '39', '39A',
                                             ifelse(DIVISION >= 25 & DIVISION <= 46 & ward_str =='39', '39B',
                                                    ifelse(DIVISION >= 29 & DIVISION <= 38 & ward_str =='40', '40A', 
                                                           ifelse(DIVISION >= 40 & DIVISION <= 59 & ward_str =='40', '40A',
                                                                  ifelse(DIVISION <= 28 & ward_str =='40', '40B', 
                                                                         ifelse(DIVISION == 39 & ward_str =='40', '40B', 
                                                                                ifelse(DIVISION == 22 & (ward_str =='40B'| ward_str =='40'), '40A',
                                                                                       ifelse(DIVISION >= 19 & DIVISION <= 46 & ward_str == '66', '66A',
                                                                                              ifelse(DIVISION <=18 & ward_str == '66', '66B', 
                                                                                                     ifelse((DIVISION == 34 | DIVISION == 41) & (ward_str == '66' | ward_str == '66A' | ward_str == '66B'), '66B', ward_str)))))))))))
d <- d %>% mutate(cand = recode(CANDIDATE, "LAWRENCE S KRASNER"= "krasner", "RICH NEGRIN"="negrin", "JOHN O NEILL"="oneill", "JOE KHAN" = "khan", "MICHAEL W UNTERMEYER" = "untermeyer", "TARIQ KARIM EL SHABAZZ" = "shabazz", "TERESA CARR DENI"= "deni", "Write In"= "write_in", .default = NA_character_))
colnames(d)[which(names(d) == "ward_str")] <- "ward"

#total votes cast in each ward
er_votes_ward <- d %>% group_by(ward)%>% summarise(votes= sum(VOTES))%>% as.data.frame()
er_votes_ward$ward <- as.character(er_votes_ward$ward)
er_17 <- er_votes_ward #will refer to the data.frame er_17 in the voter turnout calculation


# percentage of the vote each candidate won 
cand_votes <- d %>% group_by(ward, cand) %>% summarise(votes_cand = sum(VOTES)) %>% as.data.frame()
cand_votes <- left_join(cand_votes, er_votes_ward, by = "ward")
cand_votes <- cand_votes %>% mutate(pvote = (votes_cand / votes *100))


#long to wide formatting 
long_to_wide <- cand_votes %>% subset(select = c(ward, pvote, cand))
pvote_ward <- reshape(long_to_wide, idvar = "ward", timevar = "cand", direction = "wide")
## replacing . with _ in order to be ArcMap friendly column names 
colnames(pvote_ward)[which(names(pvote_ward) == "pvote.khan")] <- "pvote_khan"
colnames(pvote_ward)[which(names(pvote_ward) == "pvote.oneill")] <- "pvote_oneill"
colnames(pvote_ward)[which(names(pvote_ward) == "pvote.krasner")] <- "pvote_krasner"
colnames(pvote_ward)[which(names(pvote_ward) == "pvote.untermeyer")] <- "pvote_untermeyer"
colnames(pvote_ward)[which(names(pvote_ward) == "pvote.negrin")] <- "pvote_negrin"
colnames(pvote_ward)[which(names(pvote_ward) == "pvote.shabazz")] <- "pvote_shabazz"
colnames(pvote_ward)[which(names(pvote_ward) == "pvote.deni")] <- "pvote_deni"
colnames(pvote_ward)[which(names(pvote_ward) == "pvote.write_in")] <- "pvote_write_in"

write.csv(pvote_ward, "output/perc_of_vote_by_ward_wide.csv") #output dataset: percent of the vote won by each candidate per ward 



# continuing with calculating voter turnout 
##### registered voters per ward: voter file (DENOMINATOR) #######################

dat_vf <- read.csv("raw_data/2017_voter_file.csv") #this is the dataset of registered voters 

# rename relevant column names 
names(dat_vf)[names(dat_vf) == 'Registered.Voters.for.party.1'] <- 'dem_reg_voters'
names(dat_vf)[names(dat_vf) == 'Registered.Voters.for.party.2'] <- 'rep_reg_voters'
names(dat_vf)[names(dat_vf) == 'Registered.Voters.for.party.3'] <- 'oth_reg_voters'
names(dat_vf)[names(dat_vf) == 'Municipality.Breakdown.Name.1'] <- 'ward'
names(dat_vf)[names(dat_vf) == 'VTD.code'] <- 'division' # VTD == precinct == division 
names(dat_vf)[names(dat_vf) == 'Municipality.Name'] <- 'mun'

#make a subset dataframe with just the info we need (registered voters for Philadelphia)
dat_vf_clean <- subset(dat_vf, mun == 'PHILADELPHIA', select = c("mun", "division", "ward","dem_reg_voters", "rep_reg_voters", "oth_reg_voters"))
dat_vf_clean$division <- as.character(dat_vf_clean$division)
dat_vf_clean <- dat_vf_clean %>% mutate(division = str_sub(division, start = -2)) #get the last two digits of division because the first two are ward number 
dat_vf_clean$division <- as.integer(dat_vf_clean$division)
dat_vf_clean$ward <- as.character(dat_vf_clean$ward)

### match divisions to wards, wards A and B included 
#correctly allocate divisions to wards that are split into A and B 
d <- dat_vf_clean %>% mutate(ward = ifelse(division <= 24 & ward == '39', '39A',
                                           ifelse(division >= 25 & division <= 46 & ward =='39', '39B',
                                                  ifelse(division >= 29 & division <= 38 & ward =='40', '40A', 
                                                         ifelse(division >= 40 & division <= 59 & ward =='40', '40A',
                                                                ifelse(division <= 28 & ward =='40', '40B', 
                                                                       ifelse(division == 39 & ward =='40', '40B', 
                                                                              ifelse(division == 22 & (ward =='40B'| ward =='40'), '40A',
                                                                                     ifelse(division >= 19 & division <= 46 & ward == '66', '66A',
                                                                                            ifelse(division <=18 & ward == '66', '66B', 
                                                                                                   ifelse((division == 34 | division == 41) & (ward == '66' | ward == '66A' | ward == '66B'), '66B', ward)))))))))))



# group by new ward (with A and B)
dat_vf_clean <- d %>% group_by(ward) %>% summarise_if(is.numeric, sum, na.rm=TRUE) %>% as.data.frame()
dat_vf_clean$ward <- as.character(dat_vf_clean$ward)
dat_vf_17 <- dat_vf_clean

########################## bind registered voters to election results ############################## 

# er_17 : this is election results- votes per ward for Democrat District Attorney Candidates 
# dat_vf_clean  : this is registered voters- includes dems, reps, and other as well as total registered voters. 

voter_turnout <- left_join(er_17, dat_vf_17, by="ward")

voter_turnout_17 <- voter_turnout %>% mutate(pdem_vot_turnout = (votes / dem_reg_voters *100))
voter_turnout_17 <- select(voter_turnout_17,c("ward", "pdem_vot_turnout"))

write.csv(voter_turnout_17, "output/vot_turnout_17.csv", row.names = FALSE)


all_vars <- left_join(voter_turnout_17, pvote_ward, by="ward")
write.csv(all_vars, "output/vot_turnout_and_cand_pvote.csv", row.names = FALSE)








