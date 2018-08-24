#install.packages("dplyr")
#install.packages("stringr")
library(stringr)
library(dplyr)

dat <- read.csv('raw_data/C70_ACS_download_formatted.csv')

## this script cleans ACS 2012-2016 data for 
#Philadelphia census block groups and aggregates them to 
#ward boundaries using a factor table created in ArcMap 

#meaningful column names
# age, gender, age by gender 
names(dat)[names(dat) == 'SE_T001_001'] <- 'pop'
names(dat)[names(dat) == 'SE_T005_002'] <- 'male' 
names(dat)[names(dat) == 'SE_T005_015'] <- 'fem' 
names(dat)[names(dat) == 'SE_T009_003'] <- 'p18_34' 
names(dat)[names(dat) == 'SE_T009_004'] <- 'p35_64' 
names(dat)[names(dat) == 'SE_T009_005'] <- 'p65_over' 
names(dat)[names(dat) == 'SE_T010_003'] <- 'pm18_34' 
names(dat)[names(dat) == 'SE_T010_004'] <- 'pm35_64' 
names(dat)[names(dat) == 'SE_T010_005'] <- 'pm65_over'
names(dat)[names(dat) == 'SE_T011_003'] <- 'pf18_34' 
names(dat)[names(dat) == 'SE_T011_004'] <- 'pf35_64'  
names(dat)[names(dat) == 'SE_T011_005'] <- 'pf65_over'
                                  
#race                                    
names(dat)[names(dat) == 'SE_T013_002'] <- 'white' 
names(dat)[names(dat) == 'SE_T013_003'] <- 'black'  
names(dat)[names(dat) == 'SE_T013_004'] <- 'amer_nativ'
names(dat)[names(dat) == 'SE_T013_005'] <- 'asian'
names(dat)[names(dat) == 'SE_T013_006'] <- 'pac_isl' 
names(dat)[names(dat) == 'SE_T013_007'] <- 'race_other'
names(dat)[names(dat) == 'SE_T013_008'] <- 'race_two'

#ethnicity
names(dat)[names(dat) == 'SE_T014_002'] <- 'not_hisp' 
names(dat)[names(dat) == 'SE_T014_010'] <- 'hisp_lat'

#education                             
names(dat)[names(dat) == 'SE_T150_001'] <- 'pop_over25' 
names(dat)[names(dat) == 'SE_T150_002'] <- 'less_hs' 
names(dat)[names(dat) == 'SE_T150_003'] <- 'hs'  
names(dat)[names(dat) == 'SE_T150_004'] <- 'some_college'
names(dat)[names(dat) == 'SE_T150_005'] <- 'bachelor'
names(dat)[names(dat) == 'SE_T150_006'] <- 'masters'
names(dat)[names(dat) == 'SE_T150_007'] <- 'prof_school_over'
names(dat)[names(dat) == 'SE_T150_008'] <- 'doctorate'

#labor                               
names(dat)[names(dat) == 'SE_T037_001'] <- 'pop_laborforce' 
names(dat)[names(dat) == 'SE_T037_002'] <- 'employed' 
names(dat)[names(dat) == 'SE_T037_003'] <- 'unemployed' 

#income 
names(dat)[names(dat) == 'SE_T057_001'] <- 'med_HHincome' 

#format fips for joining 
names(dat)[names(dat) == 'Geo_GEOID'] <- 'fips'
dat$fips <- substring(dat$fips, 8)

#subset the dataframe for only useful columns
dat_census <- dat %>% select(fips, pop,male,fem,p18_34,p35_64,p65_over,pm18_34,pm35_64,pm65_over,pf18_34,pf35_64,pf65_over,white,black,amer_nativ,asian,pac_isl,race_other,race_two,not_hisp, hisp_lat,pop_over25,less_hs,hs,some_college,bachelor,masters,prof_school_over,doctorate,pop_laborforce,employed,unemployed,med_HHincome)


################# AGREGGATION FACTOR TABLE  
#this table was created in ArcMap, using the union tool with inputs of ward boundary and census block group boundary 
#in the union shapefile attribute table, a new field is created for a new area for each block group (the area that falls within a ward)
#dividing orginal block group area by new block group area gives a factor (percentage of ward covered by each block group)
#now we multiply that factor by census variables for the block group 

dat_factor <- read.csv('raw_data/aggregation_ratio_table_withAB.csv')

#format fips for joining 
dat_factor$GEOID <- as.character(dat_factor$GEOID)
dat_factor$ward_str <- as.character(dat_factor$ward_str)
names(dat_factor)[names(dat_factor) == 'ward_str'] <- 'ward'
names(dat_factor)[names(dat_factor) == 'GEOID'] <- 'fips'
names(dat_factor)[names(dat_factor) == 'ratio'] <- 'f'

#join based on fips to start making dataset of census data aggregated by neighborhood
dat_agg <- merge(dat_census, dat_factor, by ="fips")
#exclude extra noise
dat_agg <- subset(dat_agg, select = -c(FID, AFFGEOID, Shape_Leng, Shape_Area, STATEFP, 
                                       COUNTYFP, TRACTCE, BLKGRPCE, NAME, LSAD, ALAND, AWATER, FID_philly, OBJECTID, FID_wards_))


# take out medHHincome (I'll group by and find mean instead of sum for this variable) 
dat_agg_sum <- subset(dat_agg, select = -c(med_HHincome))

#mulitply by factor for all the denominators 
dat_agg_sum <- dat_agg_sum %>% mutate(fpop = floor(pop * f))%>% mutate(fpop_over25 = floor(pop_over25 *f)) %>% mutate(fpop_labforce = floor(pop_laborforce *f))  

#multiply census variables by factor to weight them appropriately (according to how much of their areas falls within the ward boundary)
dat_agg_sum <- 
  #pop by gender
  dat_agg_sum %>% mutate(fmale =male *f) %>% mutate(ffem =fem *f) %>% 
  #pop by age
  mutate(fp18_34 =p18_34 *f) %>% mutate(fp35_64 =p35_64 *f) %>% mutate(fp65_over =p65_over *f) %>% 
  #pop by gender and age 
  mutate(fpm18_34 =pm18_34 *f) %>% mutate(fpm35_64 =pm35_64 *f) %>% mutate(fpm65_over =pm65_over *f) %>% 
  mutate(fpf18_34 =pf18_34 *f) %>% mutate(fpf35_64 =pf35_64 *f) %>% mutate(fpf65_over =pf65_over *f) %>% 
  #race
  mutate(fwhite =white *f) %>% mutate(fblack =black*f) %>% 
  mutate(famer_nativ =amer_nativ *f) %>% mutate(fasian =asian *f) %>% mutate(fpac_isl =pac_isl *f) %>% 
  mutate(frace_other =race_other *f) %>% mutate(frace_two =race_two *f) %>%
  #ethnicity
  mutate(fnot_hisp = f * not_hisp) %>% mutate(fhisp_lat = f*hisp_lat) %>% 
  #education 
  mutate(fless_hs =less_hs *f) %>% mutate(fhs =hs *f) %>% mutate(fsome_college =some_college *f) %>% 
  mutate(fbachelor =bachelor *f) %>% mutate(fmasters =masters *f) %>% mutate(fprof_school_over =prof_school_over *f) %>% 
  mutate(fdoctorate =doctorate *f) %>%
  #employment 
  mutate(femployed =employed *f) %>% mutate(funemployed =unemployed *f) 
#resulting table is now counts ready to be summed by ward 


#formatting 
dat_agg_sum <- dat_agg_sum %>% mutate_if(is.numeric, as.integer)

#summarize on ward
dat_agg_sum <- dat_agg_sum %>% mutate_all(funs(replace(., is.na(.), 0)))%>% group_by(ward) %>% summarize_all(mean) #average the percent value for each census block to get an average percent value for the ward 


#create percentages 
dat_agg_sum <- # basic structure: percent value = 100 *factored value numerator / factored value denominator *100
  #pop by gender
  dat_agg_sum %>% mutate(pmale = 100* fmale/fpop) %>% mutate(pfem = 100* ffem/fpop) %>% 
  #pop by age
  mutate(pp18_34 = 100* fp18_34/fpop) %>% mutate(pp35_64 = 100* fp35_64 /fpop) %>% mutate(pp65_over = 100* fp65_over/fpop) %>% 
  #pop by gender and age 
  mutate(ppm18_34 = 100* fpm18_34 /fpop) %>% mutate(ppm35_64 = 100* fpm35_64 /fpop) %>% mutate(ppm65_over = 100* fpm65_over/fpop) %>% 
  mutate(ppf18_34 = 100* fpf18_34 /fpop) %>% mutate(ppf35_64 = 100* fpf35_64 /fpop) %>% mutate(ppf65_over = 100* fpf65_over/fpop) %>% 
  #race
  mutate(pwhite = 100* fwhite /fpop) %>% mutate(pblack = 100* fblack/fpop) %>% 
  mutate(pamer_nativ = 100* famer_nativ /fpop) %>% mutate(pasian = 100* fasian /fpop) %>% mutate(ppac_isl = 100* fpac_isl /fpop) %>% 
  mutate(prace_other =100*frace_other /fpop) %>% mutate(prace_two =100*frace_two /fpop) %>%
   #eth
  mutate(pnot_hisp = 100* fnot_hisp / fpop) %>% mutate(phisp_lat = 100* fhisp_lat / fpop) %>%
  #education 
  mutate(pless_hs = 100*fless_hs/fpop_over25) %>% mutate(phs = 100* fhs/fpop_over25) %>% mutate(psome_college = 100* fsome_college /fpop_over25) %>% 
  mutate(pbachelor = 100* fbachelor/fpop_over25) %>% mutate(pmasters = 100* fmasters/fpop_over25) %>% mutate(pprof_school_over = 100* fprof_school_over /fpop_over25) %>% 
  mutate(pdoctorate =100*fdoctorate /fpop_over25) %>%
  #employment 
  mutate(pemployed = 100* femployed /fpop_labforce) %>% mutate(punemployed = 100* funemployed /fpop_labforce) 
#^ resulting table is now in percent (numeric type)



#drop unnecessary columns 
dat_agg_sum <- subset(dat_agg_sum, select= c(ward,pmale,pfem,pp18_34,pp35_64,pp65_over,ppm18_34,ppm35_64,ppm65_over,ppf18_34,ppf35_64,ppf65_over,pwhite,pblack,pamer_nativ,pasian,ppac_isl,prace_other,prace_two,pnot_hisp,phisp_lat,pless_hs,phs,psome_college,pbachelor,pmasters,pprof_school_over,pdoctorate,pemployed,punemployed))

#join in medHHincome 
dat_agg_medHHincome <- subset(dat_agg, select = c(ward, med_HHincome))
dat_agg_medHHincome <- dat_agg_medHHincome %>% group_by(ward) %>% summarize(avg_medHHinc = mean(med_HHincome, na.rm= TRUE))

dat_clean <- merge(dat_agg_medHHincome, dat_agg_sum, by = "ward")

#export summary tables by ward for various census variables 
#this table is by ward, with 31 census variables attached, wards split into A and B are noted 

dat_clean <- dat_clean[!(dat_clean$ward == " "),]

write.csv(dat_clean, 'output/census_var_by_ward.csv', row.names = FALSE)


