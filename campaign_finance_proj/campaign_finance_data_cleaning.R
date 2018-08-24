install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)

# script description------------------------ 
#this script will: 
  # 1)  clean YTD files for Cycle associated with DA 2017 election
  # 2) group together each ward related filing 
  # 3) group together each candidate related filing 
  # 4) create new columns in the dataset to denote whether the filer or entity was candidate related or ward related
  # the output (campaign_finance_clean.csv will be used as input for the amended_filtering_script )




#  cleaning for cycle, year, and amended rows ---------------------- 
dat17 <- read.csv('raw_data/explorer_transactions_2017.csv')
dat16 <- read.csv('raw_data/explorer_transactions_2016.csv')
#removing the last row because it was a sum value in the original dataset 
dat17 <- dat17[1:(nrow(dat17) - 1), ]
dat16 <-dat16[1:(nrow(dat16) - 1), ]

#filter for primary cycles 
  # filter cycle 7 out of 2016 
  dat16 <- dat16 %>% filter(Cycle == 7)
  
  #filter cycle 1, 2, 3, out of 2017 
  dat17 <- dat17 %>% filter(Cycle == 1 | Cycle == 2 | Cycle == 3)
  
#combine 2016 and 2017 
dat <- rbind(dat16, dat17)

#take out 2015 because it's not relevant 
dat <- dat %>% filter(Year == 2016 | Year == 2017)


# ward grouping ---------------

#ward 1
  #filter FilerNames for ward 1  
w1_f <- dat %>% filter(grepl("\\b1st\\b", FilerName, ignore.case = TRUE)| grepl("\\b1\\b", FilerName) | 
                           grepl("\\bfirst\\b", FilerName, ignore.case=TRUE) | grepl("\\bone\\b", FilerName, ignore.case=TRUE))

w1_f <- w1_f %>% filter(!grepl("Students First PAC", FilerName) )

  #filter EntityNames for ward 1 
w1_e <- dat %>% filter( grepl("\\b1st\\b", EntityName, ignore.case = TRUE)| grepl("\\b1\\b", EntityName) | 
                           grepl("\\bfirst\\b", EntityName, ignore.case=TRUE) | grepl("\\bone\\b", EntityName, ignore.case=TRUE))
w1_e <- w1_e %>% filter(!grepl("Thirty First Ward", EntityName, ignore.case = TRUE) & 
                                !grepl("2 in One Breakfast & Pizza", EntityName, ignore.case = TRUE) &
                                !grepl("Demolay Consistory #1", EntityName, ignore.case = TRUE) & 
                                !grepl("Bricklayers & Allied Craftworkers", EntityName, ignore.case = TRUE) & 
                                !grepl("Bricklayers and Allied Craftworkers", EntityName, ignore.case = TRUE) & 
                                !grepl("New Gen 1 Political Action Committee 08-10 (1591)", EntityName, ignore.case = TRUE) & 
                                !grepl("One UN New York", EntityName, ignore.case = TRUE)& 
                                !grepl("Thirty First", EntityName, ignore.case = TRUE) & 
                                !grepl("Pennsylvania First", EntityName, ignore.case = TRUE) & 
                                !grepl("PENNSLYVANIA FIRST", EntityName, ignore.case = TRUE)& 
                                !grepl("First National Bank", EntityName, ignore.case = TRUE)& 
                                !grepl("First State Women", EntityName, ignore.case = TRUE)& 
                                !grepl("Students First PAC", EntityName, ignore.case = TRUE)& 
                                !grepl("New Gen 1 Political Action Committee", EntityName, ignore.case = TRUE)& 
                                !grepl("First Class Caterering Inc.", EntityName, ignore.case = TRUE) & 
                                !grepl("Bricklayers & Allied Craftworkers Local 1 Pac Fund", EntityName, ignore.case = TRUE) & 
                                !grepl("COMCAST CABLE AREA ONE", EntityName, ignore.case = TRUE)& 
                                !grepl("First Masonic District", EntityName, ignore.case = TRUE)& 
                                !grepl("Plus One Marketing Group", EntityName, ignore.case = TRUE)& 
                                !grepl("1 Brown Street Associates LP", EntityName, ignore.case = TRUE)& 
                                !grepl("BAC LOCAL 1", EntityName, ignore.case = TRUE) & 
                                !grepl("Collingdale Fire Company No. 1", EntityName, ignore.case = TRUE) & 
                                !grepl("1st Colonial Community Bank", EntityName, ignore.case = TRUE)& 
                                !grepl("CAPITAL ONE", EntityName, ignore.case = TRUE)& 
                                !grepl("First Montgomery Mgt", EntityName, ignore.case = TRUE)& 
                                !grepl("Yeadon First", EntityName, ignore.case = TRUE)& 
                                !grepl("PA First", EntityName, ignore.case = TRUE) & 
                                !grepl("We Get It Right The First Time", EntityName, ignore.case = TRUE))

  #create a master list (w1_names) of names that ward 1 has been accounted for as 
w1_names <- c(as.vector(unique(w1_f$FilerName)),as.vector(unique(w1_e$EntityName) )) 
w1_names <- unique(w1_names)

# ward 2 
w2_f <- dat %>% filter(grepl("\\b2nd\\b", FilerName, ignore.case=TRUE) | grepl("second", FilerName, ignore.case=TRUE) | 
                          grepl("\\b2\\b", FilerName)| grepl("two", FilerName, ignore.case=TRUE))
w2_f <- w2_f %>% filter(!grepl("BRICKLAYERS & ALLIED CRAFTWORKERS PAC", FilerName) & 
                            !grepl("neighborhood networks PAC", FilerName)) 

w2_e <-  dat %>% filter(grepl("\\b2nd\\b", EntityName, ignore.case=TRUE) | grepl("\\bsecond\\b", EntityName, ignore.case=TRUE) | 
                   grepl("\\b2\\b", EntityName)| grepl("\\btwo\\b", EntityName, ignore.case=TRUE))

w2_e <- w2_e %>% filter(!grepl("BRICKLAYERS & ALLIED CRAFTWORKERS PAC", EntityName, ignore.case = TRUE) & 
                                !grepl("2 in One Breakfast & Pizza", EntityName, ignore.case = TRUE) &
                                !grepl("2nd Police District Advisory Council", EntityName, ignore.case = TRUE)& 
                                !grepl("2 Street Tavern LLC", EntityName, ignore.case = TRUE)&
                                !grepl("A 2 Z Delivery Systems", EntityName, ignore.case = TRUE)& 
                                !grepl("Healthy Bites 2 Go", EntityName, ignore.case = TRUE)) 


w2_names <- unique(c(as.vector(unique(w2_f$FilerName)),as.vector(unique(w2_e$EntityName) ))) 


#ward 3 
w3_f <- dat %>% filter(grepl("\\b3rd\\b", FilerName, ignore.case=TRUE) | grepl("third", FilerName, ignore.case=TRUE) | 
                          grepl("\\b3\\b", FilerName)| grepl("three", FilerName, ignore.case=TRUE))
w3_f <- w3_f %>% filter(!grepl("Philadelphia 3.0 PAC", FilerName) ) 

w3_e <- dat %>% filter(grepl("\\b3rd\\b", EntityName, ignore.case=TRUE) | grepl("third", EntityName, ignore.case=TRUE) | 
                         grepl("\\b3\\b", EntityName)| grepl("three", EntityName, ignore.case=TRUE))

w3_e <- w3_e %>% filter(!grepl("Philadelphia 3.0 PAC", EntityName) & 
                          !grepl("JAMES  C. CRUMLISH 3RD", EntityName) & 
                          !grepl("Ironworkers Local #3", EntityName) & 
                          !grepl("3rd Police District PDAC", EntityName) & 
                          !grepl("C. A. O'CONNELL 3RD", EntityName) ) 

w3_names <- unique(c(as.vector(unique(w3_f$FilerName)),as.vector(unique(w3_e$EntityName) ))) 

# ward 4 
w4_f <- dat %>% filter(grepl("\\b4th\\b", FilerName, ignore.case=TRUE) | grepl("fourth", FilerName, ignore.case=TRUE) | 
                          grepl("\\b4\\b", FilerName)| grepl("four", FilerName, ignore.case=TRUE))

  ## NONE 
w4_e <- dat %>% filter(grepl("\\b4th\\b", EntityName, ignore.case=TRUE) | grepl("fourth", EntityName, ignore.case=TRUE) | 
                         grepl("\\b4\\b", EntityName)| grepl("four", EntityName, ignore.case=TRUE))
w4_e <- w4_e %>% filter(!grepl("Muslims 4 Humanity", EntityName) & 
                          !grepl("Four Diamonds Fund", EntityName) & 
                          !grepl("Charles Fournier", EntityName) & 
                          !grepl("Area 4", EntityName) & 
                          !grepl("Christina Fournaris", EntityName) &
                          !grepl("FOUR SEASONS", EntityName) & 
                          !grepl("FOURTH RIVER DESIGN WORK", EntityName) & 
                          !grepl("CHRISTINA DI CICCO 4 JUDGE", EntityName) & 
                          !grepl("SIMS 4 PA", EntityName) & 
                          !grepl("Armstrong 4 Executive", EntityName) & 
                          !grepl("Four Sons Pizzza", EntityName) & 
                          !grepl("Genesis 4", EntityName) & 
                          !grepl("Citizens Networking 4 Progress PAC", EntityName) & 
                          !grepl("Famous 4th Street Deli", EntityName) & 
                          !grepl("Jessica Balfour", EntityName) & 
                          !grepl("Lawncrest 4th of July Committee", EntityName) & 
                          !grepl("Four Seasons Diner", EntityName)& 
                          !grepl("Sims 4 PA PAC", EntityName)) 

w4_names <- unique(c(as.vector(unique(w4_f$FilerName)),as.vector(unique(w4_e$EntityName) ))) 
  ## NONE 

#ward 5 
w5_f <- dat %>% filter(grepl("\\b5th\\b", FilerName, ignore.case=TRUE) | grepl("fifth", FilerName, ignore.case=TRUE)| 
                          grepl("\\b5\\b", FilerName)| grepl("five", FilerName, ignore.case=TRUE))
w5_f <- w5_f %>% filter(!grepl("Fraternal Order of Police Lodge 5 PAC Fund", FilerName) & 
                            !grepl("Local No. 5 PAC Fund", FilerName)) 

w5_e <- dat %>% filter(grepl("\\b5th\\b", EntityName, ignore.case=TRUE) | grepl("fifth", EntityName, ignore.case=TRUE)| 
                         grepl("\\b5\\b", EntityName)| grepl("five", EntityName, ignore.case=TRUE))
w5_e <- w5_e %>% filter(!grepl("Fraternal Order of Police Lodge 5 PAC Fund", EntityName) & 
                          !grepl("IUEC Local", EntityName)& 
                          !grepl("Five & Below", EntityName)& 
                          !grepl("Lodge.*5", EntityName)&                           #.* means contains both words 
                          !grepl("Local.*5", EntityName, ignore.case= TRUE)& 
                          !grepl("Police.*5", EntityName)) 

w5_names <- unique(c(as.vector(unique(w5_f$FilerName)),as.vector(unique(w5_e$EntityName) ))) 


#ward 6 
w6_f <- dat %>% filter(grepl("\\b6th\\b", FilerName, ignore.case=TRUE) | grepl("sixth", FilerName, ignore.case=TRUE)| 
                          grepl("\\b6\\b", FilerName)| grepl("six", FilerName, ignore.case=TRUE))

w6_e <- dat %>% filter(grepl("\\b6th\\b", EntityName, ignore.case=TRUE) | grepl("sixth", EntityName, ignore.case=TRUE)| 
                         grepl("\\b6\\b", EntityName)| grepl("six", EntityName, ignore.case=TRUE))
w6_e <- w6_e %>% filter(!grepl("Sixsmith", EntityName) & 
                          !grepl("Pennsylvania 6", EntityName)& 
                          !grepl("Clint", EntityName)& 
                          !grepl("Six Workers", EntityName)&                           
                          !grepl("Restaurant", EntityName, ignore.case= TRUE)& 
                          !grepl("Pizza", EntityName)) 

w6_names <- unique(c(as.vector(unique(w6_f$FilerName)),as.vector(unique(w6_e$EntityName) ))) 

#ward 7 
w7_f <- dat %>% filter(grepl("\\b7th\\b", FilerName, ignore.case=TRUE) | grepl("seventh", FilerName, ignore.case=TRUE)| 
                          grepl("\\b7\\b", FilerName)| grepl("seven", FilerName, ignore.case=TRUE))
w7_e <- dat %>% filter(grepl("\\b7th\\b", EntityName, ignore.case=TRUE) | grepl("seventh", EntityName, ignore.case=TRUE)| 
                         grepl("\\b7\\b", EntityName)| grepl("seven", EntityName, ignore.case=TRUE))
w7_e <- w7_e %>% filter(!grepl(".*Eleven", EntityName) & 
                          !grepl("Seven Street Community", EntityName)& 
                          !grepl("Hoops 24-7", EntityName)& 
                          !grepl("Six Workers", EntityName))
w7_names <- unique(c(as.vector(unique(w7_f$FilerName)),as.vector(unique(w7_e$EntityName) ))) 


#ward 8 
w8_f <- dat %>% filter(grepl("\\b8th\\b", FilerName, ignore.case=TRUE) | grepl("eighth", FilerName, ignore.case=TRUE) |
                          grepl("\\b8\\b", FilerName)| grepl("eight", FilerName, ignore.case=TRUE))

w8_f <- w8_f %>% filter(!grepl("IATSE Local 8 PAC", FilerName, ignore.case=TRUE))
w8_e <- dat %>% filter(grepl("\\b8th\\b", EntityName, ignore.case=TRUE) | grepl("eighth", EntityName, ignore.case=TRUE) |
                         grepl("\\b8\\b", EntityName)| grepl("eight", EntityName, ignore.case=TRUE))

w8_e <- w8_e %>% filter(!grepl("District", EntityName) & 
                          !grepl("Heights", EntityName)& 
                          !grepl("Penn Center", EntityName)& 
                          !grepl("Lodge.*5", EntityName)&      
                          !grepl("Rouge", EntityName)& 
                          !grepl("Local.*8", EntityName, ignore.case= TRUE)) 

w8_names <- unique(c(as.vector(unique(w8_f$FilerName)),as.vector(unique(w8_e$EntityName) ))) 

#ward 9 
w9_f <- dat %>% filter(grepl("\\b9th\\b", FilerName, ignore.case=TRUE) | grepl("ninth", FilerName, ignore.case=TRUE) | 
                          grepl("\\b9\\b", FilerName)| grepl("nine", FilerName, ignore.case=TRUE))
w9_e <- dat %>% filter(grepl("\\b9th\\b", EntityName, ignore.case=TRUE) | grepl("ninth", EntityName, ignore.case=TRUE) | 
                         grepl("\\b9\\b", EntityName)| grepl("nine", EntityName, ignore.case=TRUE))
w9_e <- w9_e %>% filter(!grepl("J.*nine", EntityName, ignore.case =TRUE) & 
                          !grepl("Ancient Order", EntityName)& 
                          !grepl("Walsh", EntityName, ignore.case = TRUE)& 
                          !grepl("Strategies", EntityName)&      
                          !grepl("Rouge", EntityName)) 

w9_names <- unique(c(as.vector(unique(w9_f$FilerName)),as.vector(unique(w9_e$EntityName) ))) 

# ward 10 
w10_f <- dat %>% filter(grepl("\\b10th\\b", FilerName, ignore.case=TRUE) | grepl("tenth", FilerName, ignore.case=TRUE) | 
                           grepl("\\b10\\b", FilerName)| grepl("ten", FilerName, ignore.case=TRUE))

w10_e <- dat %>% filter(grepl("\\b10th\\b", EntityName, ignore.case=TRUE) | grepl("^tenth", EntityName, ignore.case=TRUE) | 
                          grepl("\\b10\\b", EntityName)| grepl("^ten", EntityName, ignore.case=TRUE))
w10_e <- w10_e %>% filter(!grepl("embassy suites", EntityName, ignore.case =TRUE) & 
                          !grepl("Tents", EntityName)& 
                          !grepl("New Gen", EntityName, ignore.case = TRUE)& 
                          !grepl("tenille", EntityName, ignore.case = TRUE)&      
                          !grepl("tenet", EntityName, ignore.case = TRUE )&
                          !grepl("part time workers", EntityName, ignore.case = TRUE) ) 
w10_names <- unique(c(as.vector(unique(w10_f$FilerName)),as.vector(unique(w10_e$EntityName) ))) 


# ward 11 
w11_f <- dat %>% filter(grepl("\\b11th\\b", FilerName, ignore.case=TRUE) | grepl("eleventh", FilerName, ignore.case=TRUE) 
                         | grepl("\\b11\\b", FilerName)| grepl("eleven", FilerName, ignore.case=TRUE))
w11_f <- w11_f %>% filter(!grepl("Bobby 11", FilerName, ignore.case=TRUE))
w11_e <- dat %>% filter(grepl("\\b11th\\b", EntityName, ignore.case=TRUE) | grepl("eleventh", EntityName, ignore.case=TRUE) 
                        | grepl("\\b11\\b", EntityName)| grepl("eleven", EntityName, ignore.case=TRUE))
w11_e <- w11_e %>% filter(!grepl("bobby", EntityName, ignore.case =TRUE) & 
                            !grepl("Seven", EntityName)& 
                            !grepl("7", EntityName, ignore.case = TRUE)& 
                            !grepl("area", EntityName, ignore.case = TRUE) ) 
w11_names <- unique(c(as.vector(unique(w11_f$FilerName)),as.vector(unique(w11_e$EntityName) ))) 

  
# ward 12 
w12_f <- dat %>% filter(grepl("\\b12th\\b", FilerName, ignore.case=TRUE) | grepl("twelfth", FilerName, ignore.case=TRUE) 
                         | grepl("\\b12\\b", FilerName)| grepl("twelve", FilerName, ignore.case=TRUE))
w12_e <- dat %>% filter(grepl("\\b12th\\b", EntityName, ignore.case=TRUE) | grepl("twelfth", EntityName, ignore.case=TRUE) 
                        | grepl("\\b12\\b", EntityName)| grepl("twelve", EntityName, ignore.case=TRUE))
w12_e <- w12_e %>% filter(!grepl("parking", EntityName, ignore.case =TRUE)) 

w12_names <- unique(c(as.vector(unique(w12_f$FilerName)),as.vector(unique(w12_e$EntityName) ))) 

# ward 13  
w13_f <- dat %>% filter(grepl("\\b13th\\b", FilerName, ignore.case=TRUE) | grepl("thirteenth", FilerName, ignore.case=TRUE) 
                         | grepl("\\b13\\b", FilerName)| grepl("thirteen", FilerName, ignore.case=TRUE))
w13_e <- dat %>% filter(grepl("\\b13th\\b", EntityName, ignore.case=TRUE) | grepl("thirteenth", EntityName, ignore.case=TRUE) 
                        | grepl("\\b13\\b", EntityName)| grepl("thirteen", EntityName, ignore.case=TRUE))
w13_e <- w13_e %>% filter(!grepl(".*boiler.*", EntityName, ignore.case =TRUE) & 
                            !grepl("afscme", EntityName, ignore.case=TRUE ) )
w13_names <- unique(c(as.vector(unique(w13_f$FilerName)),as.vector(unique(w13_e$EntityName) ))) 


# ward 14 
w14_f <- dat %>% filter(grepl("\\b14th\\b", FilerName, ignore.case=TRUE) | grepl("fourteenth", FilerName, ignore.case=TRUE) 
                         | grepl("\\b14\\b", FilerName)| grepl("fourteen", FilerName, ignore.case=TRUE))
w14_e <- dat %>% filter(grepl("\\b14th\\b", EntityName, ignore.case=TRUE) | grepl("fourteenth", EntityName, ignore.case=TRUE) 
                        | grepl("\\b14\\b", EntityName)| grepl("fourteen", EntityName, ignore.case=TRUE))

w14_e <- w14_e %>% filter(!grepl("Local.*14", EntityName, ignore.case= TRUE)) 

w14_names <- unique(c(as.vector(unique(w14_f$FilerName)),as.vector(unique(w14_e$EntityName) ))) 


# ward 15 
w15_f <- dat %>% filter(grepl("\\b15th\\b", FilerName, ignore.case=TRUE) | grepl("fifteenth", FilerName, ignore.case=TRUE) 
                         | grepl("\\b15\\b", FilerName)| grepl("fifteen", FilerName, ignore.case=TRUE))

w15_e <- dat %>% filter(grepl("\\b15th\\b", EntityName, ignore.case=TRUE) | grepl("fifteenth", EntityName, ignore.case=TRUE) 
                        | grepl("\\b15\\b", EntityName)| grepl("fifteen", EntityName, ignore.case=TRUE))
w15_e <- w15_e %>% filter(!grepl("deeley", EntityName, ignore.case= TRUE)&
                            !grepl("pdac", EntityName, ignore.case= TRUE)) 

w15_names <- unique(c(as.vector(unique(w15_f$FilerName)),as.vector(unique(w15_e$EntityName) ))) 

#ward 16 
w16_f <- dat %>% filter(grepl("\\b16th\\b", FilerName, ignore.case=TRUE) | grepl("sixteenth", FilerName, ignore.case=TRUE) 
                         | grepl("\\b16\\b", FilerName)| grepl("sixteen", FilerName, ignore.case=TRUE))
w16_e <- dat %>% filter(grepl("\\b16th\\b", EntityName, ignore.case=TRUE) | grepl("sixteenth", EntityName, ignore.case=TRUE) 
                        | grepl("\\b16\\b", EntityName)| grepl("sixteen", EntityName, ignore.case=TRUE))
w16_names <- unique(c(as.vector(unique(w16_f$FilerName)),as.vector(unique(w16_e$EntityName) ))) 

#ward 17 
w17_f <- dat %>% filter(grepl("\\b17th\\b", FilerName, ignore.case=TRUE) | grepl("seventeenth", FilerName, ignore.case=TRUE) 
                         | grepl("\\b17\\b", FilerName)| grepl("seventeen", FilerName, ignore.case=TRUE))
w17_e <- dat %>% filter(grepl("\\b17th\\b", EntityName, ignore.case=TRUE) | grepl("seventeenth", EntityName, ignore.case=TRUE) 
                        | grepl("\\b17\\b", EntityName)| grepl("seventeen", EntityName, ignore.case=TRUE))
w17_e <- w17_e %>% filter(!grepl("division", EntityName, ignore.case= TRUE)&
                            !grepl("fund", EntityName, ignore.case= TRUE)& 
                            !grepl("laoh", EntityName, ignore.case= TRUE) &
                          !grepl("community center", EntityName, ignore.case= TRUE)) 
w17_names <- unique(c(as.vector(unique(w17_f$FilerName)),as.vector(unique(w17_e$EntityName) ))) 


#ward 18 
w18_f <- dat %>% filter(grepl("\\b18th\\b", FilerName, ignore.case=TRUE) | grepl("eighteenth", FilerName, ignore.case=TRUE) 
                         | grepl("\\b18\\b", FilerName)| grepl("eighteen", FilerName, ignore.case=TRUE))
w18_e <- dat %>% filter(grepl("\\b18th\\b", EntityName, ignore.case=TRUE) | grepl("eighteenth", EntityName, ignore.case=TRUE) 
                        | grepl("\\b18\\b", EntityName)| grepl("eighteen", EntityName, ignore.case=TRUE))
w18_names <- unique(c(as.vector(unique(w18_f$FilerName)),as.vector(unique(w18_e$EntityName) ))) 


#ward 19
w19_f <- dat %>% filter(grepl("\\b19th\\b", FilerName, ignore.case=TRUE) | grepl("nineteenth", FilerName, ignore.case=TRUE) |
                           grepl("\\b19\\b", FilerName)| grepl("nineteen", FilerName, ignore.case=TRUE))
w19_f <- w19_f %>% filter(!grepl("Sheet Metal Workers Local 19 PEL PA & NJ", FilerName, ignore.case=TRUE))
w19_e <- dat %>% filter(grepl("\\b19th\\b", EntityName, ignore.case=TRUE) | grepl("nineteenth", EntityName, ignore.case=TRUE) |
                          grepl("\\b19\\b", EntityName)| grepl("nineteen", EntityName, ignore.case=TRUE))
w19_e <- w19_e %>% filter(!grepl("sheet.*metal", EntityName, ignore.case=TRUE))
w19_names <- unique(c(as.vector(unique(w19_f$FilerName)),as.vector(unique(w19_e$EntityName) ))) 



#ward 20
w20_f <- dat %>% filter(grepl("\\b20th\\b", FilerName, ignore.case=TRUE) | grepl("twentieth", FilerName, ignore.case=TRUE) | 
                           grepl("\\b20\\b", FilerName)| grepl("twenty", FilerName, ignore.case=TRUE))

w20_e <- dat %>% filter(grepl("\\b20th\\b", EntityName, ignore.case=TRUE) | grepl("twentieth", EntityName, ignore.case=TRUE) | 
                          grepl("\\b20\\b", EntityName)| grepl("twenty", EntityName, ignore.case=TRUE))

w20_names <- unique(c(as.vector(unique(w20_f$FilerName)),as.vector(unique(w20_e$EntityName) ))) 


# ward 21 
w21_f <- dat %>% filter(grepl("\\b21st\\b", FilerName, ignore.case=TRUE) | grepl("twentyfirst", FilerName, ignore.case=TRUE) | 
                           grepl("twenty-first", FilerName, ignore.case=TRUE)|grepl("\\b21\\b", FilerName)| 
                           grepl("twenty first", FilerName, ignore.case=TRUE)|| grepl("twenty one", FilerName, ignore.case=TRUE) | 
                           grepl("twentyone", FilerName, ignore.case=TRUE))


w21_e <- dat %>% filter(grepl("\\b21st\\b", EntityName, ignore.case=TRUE) | grepl("twentyfirst", EntityName, ignore.case=TRUE) | 
                          grepl("twenty-first", EntityName, ignore.case=TRUE)|grepl("\\b21\\b", EntityName)| 
                          grepl("twenty first", EntityName, ignore.case=TRUE)|| grepl("twenty one", EntityName, ignore.case=TRUE) | 
                          grepl("twentyone", EntityName, ignore.case=TRUE))
w21_names <- unique(c(as.vector(unique(w21_f$FilerName)),as.vector(unique(w21_e$EntityName) ))) 
 # NONE 


# ward 22 
w22_f <- dat %>% filter(grepl("\\b22nd\\b", FilerName, ignore.case=TRUE) | grepl("twentysecond", FilerName, ignore.case=TRUE) | 
                           grepl("twenty-second", FilerName, ignore.case=TRUE)| grepl("\\b22\\b", FilerName)| 
                           grepl("twenty second", FilerName, ignore.case=TRUE) | grepl("twenty two", FilerName, ignore.case=TRUE) | 
                           grepl("twentytwo", FilerName, ignore.case=TRUE))
w22_e <- dat %>% filter(grepl("\\b22nd\\b", EntityName, ignore.case=TRUE) | grepl("twentysecond", EntityName, ignore.case=TRUE) | 
                          grepl("twenty-second", EntityName, ignore.case=TRUE)| grepl("\\b22\\b", EntityName)| 
                          grepl("twenty second", EntityName, ignore.case=TRUE) | grepl("twenty two", EntityName, ignore.case=TRUE) | 
                          grepl("twentytwo", EntityName, ignore.case=TRUE) )

w22_e <- w22_e %>% filter(!grepl(".*fire.*22", EntityName, ignore.case= TRUE) & 
                            !grepl("Local.*22", EntityName, ignore.case= TRUE)&
                            !grepl("street", EntityName, ignore.case= TRUE) & 
                            !grepl("-25", EntityName, ignore.case= TRUE)) 
w22_names <- unique(c(as.vector(unique(w22_f$FilerName)),as.vector(unique(w22_e$EntityName) ))) 


#ward 23 
w23_f <- dat %>% filter(grepl("\\b23rd\\b", FilerName, ignore.case=TRUE) | grepl("twentythird", FilerName, ignore.case=TRUE) | 
                           grepl("twenty-third", FilerName, ignore.case=TRUE)| grepl("\\b23\\b", FilerName)| 
                           grepl("twenty third", FilerName, ignore.case=TRUE)| grepl("twenty three", FilerName, ignore.case=TRUE)| 
                           grepl("twentythree", FilerName, ignore.case=TRUE))
w23_e <- dat %>% filter(grepl("\\b23rd\\b", EntityName, ignore.case=TRUE) | grepl("twentythird", EntityName, ignore.case=TRUE) | 
                          grepl("twenty-third", EntityName, ignore.case=TRUE)| grepl("\\b23\\b", EntityName)| 
                          grepl("twenty third", EntityName, ignore.case=TRUE)| grepl("twenty three", EntityName, ignore.case=TRUE)| 
                          grepl("twentythree", EntityName, ignore.case=TRUE))

w23_e <- w23_e %>% filter(!grepl("district", EntityName, ignore.case= TRUE) & 
                            !grepl("Local.*23", EntityName, ignore.case= TRUE)&
                            !grepl("cafe", EntityName, ignore.case= TRUE) & 
                            !grepl("ufcw", EntityName, ignore.case= TRUE)) 
w23_names <- unique(c(as.vector(unique(w23_f$FilerName)),as.vector(unique(w23_e$EntityName) ))) 


#ward 24 
w24_f <- dat %>% filter(grepl("\\b24th\\b", FilerName, ignore.case=TRUE) | grepl("twentyfourth", FilerName, ignore.case=TRUE) | 
                           grepl("twenty-fourth", FilerName, ignore.case=TRUE)| grepl("\\b24\\b", FilerName)| 
                           grepl("twenty fourth", FilerName, ignore.case=TRUE)| grepl("twenty four", FilerName, ignore.case=TRUE)| 
                           grepl("twentyfour", FilerName, ignore.case=TRUE))
w24_e <- dat %>% filter(grepl("\\b24th\\b", EntityName, ignore.case=TRUE) | grepl("twentyfourth", EntityName, ignore.case=TRUE) | 
                          grepl("twenty-fourth", EntityName, ignore.case=TRUE)| grepl("\\b24\\b", EntityName)| 
                          grepl("twenty fourth", EntityName, ignore.case=TRUE)| grepl("twenty four", EntityName, ignore.case=TRUE)| 
                          grepl("twentyfour", EntityName, ignore.case=TRUE))
w24_e <- w24_e %>% filter(!grepl(".*-7", EntityName, ignore.case= TRUE) & 
                            !grepl("pdac", EntityName, ignore.case= TRUE)) 
w24_names <- unique(c(as.vector(unique(w24_f$FilerName)),as.vector(unique(w24_e$EntityName)))) 


#ward 25 
w25_f <- dat %>% filter(grepl("\\b25th\\b", FilerName, ignore.case=TRUE) | grepl("twentyfifth", FilerName, ignore.case=TRUE) | 
                           grepl("twenty-fifth", FilerName, ignore.case=TRUE)| grepl("\\b25\\b", FilerName)| 
                           grepl("twenty fifth", FilerName, ignore.case=TRUE)| grepl("twenty five", FilerName, ignore.case=TRUE)| 
                           grepl("twentyfive", FilerName, ignore.case=TRUE))
w25_e <- dat %>% filter(grepl("\\b25th\\b", EntityName, ignore.case=TRUE) | grepl("twentyfifth", EntityName, ignore.case=TRUE) | 
                          grepl("twenty-fifth", EntityName, ignore.case=TRUE)| grepl("\\b25\\b", EntityName)| 
                          grepl("twenty fifth", EntityName, ignore.case=TRUE)| grepl("twenty five", EntityName, ignore.case=TRUE)| 
                          grepl("twentyfive", EntityName, ignore.case=TRUE))
w25_e <- w25_e %>% filter(!grepl(".*22-", EntityName, ignore.case= TRUE) & 
                            !grepl("aoh", EntityName, ignore.case= TRUE)& 
                            !grepl("a.o.h.", EntityName, ignore.case= TRUE)) 
w25_names <- unique(c(as.vector(unique(w25_f$FilerName)),as.vector(unique(w25_e$EntityName)))) 

#ward 26 
w26_f <- dat %>% filter(grepl("\\b26th\\b", FilerName, ignore.case=TRUE) | grepl("twentysixth", FilerName, ignore.case=TRUE) | 
                           grepl("twenty sixth", FilerName, ignore.case=TRUE)| grepl("\\b26\\b", FilerName)| 
                           grepl("twentysix", FilerName, ignore.case=TRUE)| grepl("twenty six", FilerName, ignore.case=TRUE)| 
                           grepl("twentysix", FilerName, ignore.case=TRUE))
w26_e <- dat %>% filter(grepl("\\b26th\\b", EntityName, ignore.case=TRUE) | grepl("twentysixth", EntityName, ignore.case=TRUE) | 
                          grepl("twenty sixth", EntityName, ignore.case=TRUE)| grepl("\\b26\\b", EntityName)| 
                          grepl("twentysix", EntityName, ignore.case=TRUE)| grepl("twenty six", EntityName, ignore.case=TRUE)| 
                          grepl("twentysix", EntityName, ignore.case=TRUE))
w26_e <- w26_e %>% filter(!grepl("district", EntityName, ignore.case= TRUE) & 
                            !grepl("sydenham", EntityName, ignore.case= TRUE)) 
w26_names <- unique(c(as.vector(unique(w26_f$FilerName)),as.vector(unique(w26_e$EntityName)))) 


#ward 27 
w27_f <- dat %>% filter(grepl("\\b27th\\b", FilerName, ignore.case=TRUE) | grepl("twentyseventh", FilerName, ignore.case=TRUE) | 
                           grepl("twenty-seventh", FilerName, ignore.case=TRUE)| grepl("\\b27\\b", FilerName)| 
                           grepl("twentyseven", FilerName, ignore.case=TRUE)| grepl("twenty seven", FilerName, ignore.case=TRUE)| 
                           grepl("twentyseven", FilerName, ignore.case=TRUE))
w27_e <- dat %>% filter(grepl("\\b27th\\b", EntityName, ignore.case=TRUE) | grepl("twentyseventh", EntityName, ignore.case=TRUE) | 
                          grepl("twenty-seventh", EntityName, ignore.case=TRUE)| grepl("\\b27\\b", EntityName)| 
                          grepl("twentyseven", EntityName, ignore.case=TRUE)| grepl("twenty seven", EntityName, ignore.case=TRUE)| 
                          grepl("twentyseven", EntityName, ignore.case=TRUE))
w27_e <- w27_e %>% filter(!grepl("plumber|plumbers", EntityName, ignore.case= TRUE))

w27_names <- unique(c(as.vector(unique(w27_f$FilerName)),as.vector(unique(w27_e$EntityName)))) 



#ward 28 

w28_f <- dat %>% filter(grepl("\\b28th\\b", FilerName, ignore.case=TRUE) | grepl("twentyeighth", FilerName, ignore.case=TRUE) | 
                           grepl("twenty eighth", FilerName, ignore.case=TRUE)| grepl("\\b28\\b", FilerName)| 
                           grepl("twentyeight", FilerName, ignore.case=TRUE)| grepl("twenty eight", FilerName, ignore.case=TRUE)| 
                           grepl("twentyeight", FilerName, ignore.case=TRUE))
w28_e <- dat %>% filter(grepl("\\b28th\\b", EntityName, ignore.case=TRUE) | grepl("twentyeighth", EntityName, ignore.case=TRUE) | 
                          grepl("twenty eighth", EntityName, ignore.case=TRUE)| grepl("\\b28\\b", EntityName)| 
                          grepl("twentyeight", EntityName, ignore.case=TRUE)| grepl("twenty eight", EntityName, ignore.case=TRUE)| 
                          grepl("twentyeight", EntityName, ignore.case=TRUE))
w28_names <- unique(c(as.vector(unique(w28_f$FilerName)),as.vector(unique(w28_e$EntityName)))) 

#ward 29 
w29_f <- dat %>% filter(grepl("\\b29th\\b", FilerName, ignore.case=TRUE) | grepl("twentyninth", FilerName, ignore.case=TRUE) | 
                           grepl("twenty ninth", FilerName, ignore.case=TRUE)| grepl("\\b29\\b", FilerName)| 
                           grepl("twentynine", FilerName, ignore.case=TRUE)| grepl("twenty nine", FilerName, ignore.case=TRUE)| 
                           grepl("twentynine", FilerName, ignore.case=TRUE))
w29_e <- dat %>% filter(grepl("\\b29th\\b", EntityName, ignore.case=TRUE) | grepl("twentyninth", EntityName, ignore.case=TRUE) | 
                          grepl("twenty ninth", EntityName, ignore.case=TRUE)| grepl("\\b29\\b", EntityName)| 
                          grepl("twentynine", EntityName, ignore.case=TRUE)| grepl("twenty nine", EntityName, ignore.case=TRUE)| 
                          grepl("twentynine", EntityName, ignore.case=TRUE))
w29_names <- unique(c(as.vector(unique(w29_f$FilerName)),as.vector(unique(w29_e$EntityName)))) 

#ward 30 
w30_f <- dat %>% filter(grepl("\\b03th\\b", FilerName, ignore.case=TRUE) | grepl("thirtieth", FilerName, ignore.case=TRUE) | 
                            grepl("\\b30\\b", FilerName)| grepl("thirty", FilerName, ignore.case=TRUE))
w30_f <- w30_f %>% filter(!grepl("COMPOSITION ROOFERS UNION LOCAL 30", FilerName, ignore.case=TRUE)) 
w30_e <- dat %>% filter(grepl("\\b03th\\b", EntityName, ignore.case=TRUE) | grepl("thirtieth", EntityName, ignore.case=TRUE) | 
                          grepl("\\b30\\b", EntityName)| grepl("thirty", EntityName, ignore.case=TRUE))
w30_e <- w30_e %>% filter(!grepl("roofer.*", EntityName, ignore.case=TRUE) &
                            !grepl("strategies", EntityName, ignore.case=TRUE) &
                            !grepl("kenneth", EntityName, ignore.case=TRUE))
w30_names <- unique(c(as.vector(unique(w30_f$FilerName)),as.vector(unique(w30_e$EntityName)))) 


#ward 31
w31_f <- dat %>% filter(grepl("\\b31st\\b", FilerName, ignore.case=TRUE) | grepl("thirtyfirst", FilerName, ignore.case=TRUE) | 
                             grepl("thirty-first", FilerName, ignore.case=TRUE)| grepl("\\b31\\b", FilerName)| 
                             grepl("thirty first", FilerName, ignore.case=TRUE) | grepl("thirty one", FilerName, ignore.case=TRUE)| 
                           grepl("thirtyone", FilerName, ignore.case=TRUE))
w31_e <- dat %>% filter(grepl("\\b31st\\b", EntityName, ignore.case=TRUE) | grepl("thirtyfirst", EntityName, ignore.case=TRUE) | 
                          grepl("thirty-first", EntityName, ignore.case=TRUE)| grepl("\\b31\\b", EntityName)| 
                          grepl("thirty first", EntityName, ignore.case=TRUE) | grepl("thirty one", EntityName, ignore.case=TRUE)| 
                          grepl("thirtyone", EntityName, ignore.case=TRUE))
w31_names <- unique(c(as.vector(unique(w31_f$FilerName)),as.vector(unique(w31_e$EntityName)))) 

# ward 32 
w32_f <- dat %>% filter(grepl("\\b32nd\\b", FilerName, ignore.case=TRUE) | grepl("thirtysecond", FilerName, ignore.case=TRUE) | 
                           grepl("thirty-second", FilerName, ignore.case=TRUE)| grepl("\\b32\\b", FilerName)| 
                           grepl("thirty second", FilerName, ignore.case=TRUE) | grepl("thirty two", FilerName, ignore.case=TRUE)| 
                           grepl("thirtytwo", FilerName, ignore.case=TRUE))
w32_e <- dat %>% filter(grepl("\\b32nd\\b", EntityName, ignore.case=TRUE) | grepl("thirtysecond", EntityName, ignore.case=TRUE) | 
                          grepl("thirty-second", EntityName, ignore.case=TRUE)| grepl("\\b32\\b", EntityName)| 
                          grepl("thirty second", EntityName, ignore.case=TRUE) | grepl("thirty two", EntityName, ignore.case=TRUE)| 
                          grepl("thirtytwo", EntityName, ignore.case=TRUE))
w32_e <- w32_e %>% filter(!grepl("local*", EntityName, ignore.case=TRUE))
w32_names <- unique(c(as.vector(unique(w32_f$FilerName)),as.vector(unique(w32_e$EntityName)))) 

#ward 33 
w33_f <- dat %>% filter(grepl("\\b33rd\\b", FilerName, ignore.case=TRUE) | grepl("thirtythird", FilerName, ignore.case=TRUE) | 
                           grepl("thirty-third", FilerName, ignore.case=TRUE)| grepl("\\b33\\b", FilerName)| 
                           grepl("thirty third", FilerName, ignore.case=TRUE)| grepl("thirty three", FilerName, ignore.case=TRUE)| 
                           grepl("thirtythree", FilerName, ignore.case=TRUE))
w33_f <- w33_f %>% filter(!grepl("DISTRICT COUNCIL 33 POLITICAL CONTRIBUTION SSF", FilerName, ignore.case=TRUE))
w33_e <- dat %>% filter(grepl("\\b33rd\\b", EntityName, ignore.case=TRUE) | grepl("thirtythird", EntityName, ignore.case=TRUE) | 
                          grepl("thirty-third", EntityName, ignore.case=TRUE)| grepl("\\b33\\b", EntityName)| 
                          grepl("thirty third", EntityName, ignore.case=TRUE)| grepl("thirty three", EntityName, ignore.case=TRUE)| 
                          grepl("thirtythree", EntityName, ignore.case=TRUE))
w33_e <- w33_e %>% filter(!grepl("afscme|afcme", EntityName, ignore.case=TRUE) & 
                            !grepl("dunkin", EntityName, ignore.case=TRUE))
 
w33_names <- unique(c(as.vector(unique(w33_f$FilerName)),as.vector(unique(w33_e$EntityName)))) 

#ward 34  
w34_f <- dat %>% filter(grepl("\\b34th\\b", FilerName, ignore.case=TRUE) | grepl("thirtyfourth", FilerName, ignore.case=TRUE) | 
                           grepl("thirty-fourth", FilerName, ignore.case=TRUE)| grepl("\\b34\\b", FilerName)| 
                           grepl("thirty fourth", FilerName, ignore.case=TRUE)| grepl("thirty four", FilerName, ignore.case=TRUE)| 
                           grepl("thirtyfour", FilerName, ignore.case=TRUE))
w34_e <- dat %>% filter(grepl("\\b34th\\b", EntityName, ignore.case=TRUE) | grepl("thirtyfourth", EntityName, ignore.case=TRUE) | 
                          grepl("thirty-fourth", EntityName, ignore.case=TRUE)| grepl("\\b34\\b", EntityName)| 
                          grepl("thirty fourth", EntityName, ignore.case=TRUE)| grepl("thirty four", EntityName, ignore.case=TRUE)| 
                          grepl("thirtyfour", EntityName, ignore.case=TRUE)) 
w34_e <- w34_e %>% filter(!grepl("\\bst\\b", EntityName, ignore.case=TRUE) & 
                            !grepl("local", EntityName, ignore.case=TRUE) & 
                            !grepl("workers", EntityName, ignore.case=TRUE))
## NONE 
w34_names <- unique(c(as.vector(unique(w34_f$FilerName)),as.vector(unique(w34_e$EntityName)))) 


#ward 35 
w35_f <- dat %>% filter(grepl("\\b35th\\b", FilerName, ignore.case=TRUE) | grepl("thirtyfifth", FilerName, ignore.case=TRUE) | 
                           grepl("thirty-fifth", FilerName, ignore.case=TRUE)| grepl("\\b35\\b", FilerName)| 
                           grepl("thirty fifth", FilerName, ignore.case=TRUE)| grepl("thirty five", FilerName, ignore.case=TRUE)| 
                           grepl("thirtyfive", FilerName, ignore.case=TRUE))
w35_e <- dat %>% filter(grepl("\\b35th\\b", EntityName, ignore.case=TRUE) | grepl("thirtyfifth", EntityName, ignore.case=TRUE) | 
                          grepl("thirty-fifth", EntityName, ignore.case=TRUE)| grepl("\\b35\\b", EntityName)| 
                          grepl("thirty fifth", EntityName, ignore.case=TRUE)| grepl("thirty five", EntityName, ignore.case=TRUE)| 
                          grepl("thirtyfive", EntityName, ignore.case=TRUE))
w35_e <- w35_e %>% filter(!grepl("district", EntityName, ignore.case=TRUE)) 
w35_names <- unique(c(as.vector(unique(w35_f$FilerName)),as.vector(unique(w35_e$EntityName)))) 


#ward 36 
w36_f <- dat %>% filter(grepl("\\b36th\\b", FilerName, ignore.case=TRUE) | grepl("thirtysixth", FilerName, ignore.case=TRUE) | 
                           grepl("thirty sixth", FilerName, ignore.case=TRUE)| grepl("\\b36\\b", FilerName)| 
                           grepl("thirtysix", FilerName, ignore.case=TRUE)| grepl("thirty six", FilerName, ignore.case=TRUE)| 
                           grepl("thirtysix", FilerName, ignore.case=TRUE))
w36_e <- dat %>% filter(grepl("\\b36th\\b", EntityName, ignore.case=TRUE) | grepl("thirtysixth", EntityName, ignore.case=TRUE) | 
                          grepl("thirty sixth", EntityName, ignore.case=TRUE)| grepl("\\b36\\b", EntityName)| 
                          grepl("thirtysix", EntityName, ignore.case=TRUE)| grepl("thirty six", EntityName, ignore.case=TRUE)| 
                          grepl("thirtysix", EntityName, ignore.case=TRUE))
## decided to keep "Pamela Gibson 36th Ward" included 
w36_names <- unique(c(as.vector(unique(w36_f$FilerName)),as.vector(unique(w36_e$EntityName)))) 



#ward 37 
w37_f <- dat %>% filter(grepl("\\b37th\\b", FilerName, ignore.case=TRUE) | grepl("thirtyseventh", FilerName, ignore.case=TRUE) | 
                           grepl("thirty-seventh", FilerName, ignore.case=TRUE)| grepl("\\b37\\b", FilerName)| 
                           grepl("thirtyseven", FilerName, ignore.case=TRUE)| grepl("thirty seven", FilerName, ignore.case=TRUE)| 
                           grepl("thirtyseven", FilerName, ignore.case=TRUE))
w37_e <- dat %>% filter(grepl("\\b37th\\b", EntityName, ignore.case=TRUE) | grepl("thirtyseventh", EntityName, ignore.case=TRUE) | 
                          grepl("thirty-seventh", EntityName, ignore.case=TRUE)| grepl("\\b37\\b", EntityName)| 
                          grepl("thirtyseven", EntityName, ignore.case=TRUE)| grepl("thirty seven", EntityName, ignore.case=TRUE)| 
                          grepl("thirtyseven", EntityName, ignore.case=TRUE))
w37_e <- w37_e %>% filter(!grepl("lodge", EntityName, ignore.case=TRUE)) 
w37_names <- unique(c(as.vector(unique(w37_f$FilerName)),as.vector(unique(w37_e$EntityName)))) 

#ward 38 
w38_f <- dat %>% filter(grepl("\\b38th\\b", FilerName, ignore.case=TRUE) | grepl("thirtyeighth", FilerName, ignore.case=TRUE) | 
                           grepl("thirty eighth", FilerName, ignore.case=TRUE)| grepl("\\b38\\b", FilerName)| 
                           grepl("thirtyeight", FilerName, ignore.case=TRUE)| grepl("thirty eight", FilerName, ignore.case=TRUE))
w38_e <- dat %>% filter(grepl("\\b38th\\b", EntityName, ignore.case=TRUE) | grepl("thirtyeighth", EntityName, ignore.case=TRUE) | 
                          grepl("thirty eighth", EntityName, ignore.case=TRUE)| grepl("\\b38\\b", EntityName)| 
                          grepl("thirtyeight", EntityName, ignore.case=TRUE)| grepl("thirty eight", EntityName, ignore.case=TRUE))
w38_names <- unique(c(as.vector(unique(w38_f$FilerName)),as.vector(unique(w38_e$EntityName)))) 


#ward 39 
w39_f <- dat %>% filter(grepl("\\b39th\\b", FilerName, ignore.case=TRUE) | grepl("thirtyninth", FilerName, ignore.case=TRUE) | 
                           grepl("thirty ninth", FilerName, ignore.case=TRUE)| grepl("\\b39\\b", FilerName)| 
                           grepl("thirtynine", FilerName, ignore.case=TRUE)| grepl("thirty nine", FilerName, ignore.case=TRUE))
w39_e <- dat %>% filter(grepl("\\b39th\\b", EntityName, ignore.case=TRUE) | grepl("thirtyninth", EntityName, ignore.case=TRUE) | 
                          grepl("thirty ninth", EntityName, ignore.case=TRUE)| grepl("\\b39\\b", EntityName)| 
                          grepl("thirtynine", EntityName, ignore.case=TRUE)| grepl("thirty nine", EntityName, ignore.case=TRUE))
w39_e <- w39_e %>% filter(!grepl("workers|warkers", EntityName, ignore.case=TRUE) & 
                            !grepl("aoh|a.o.h.", EntityName, ignore.case=TRUE)) 

#w39_names <- unique(c(as.vector(unique(w39_f$FilerName)),as.vector(unique(w39_e$EntityName)))) 

a1 <- "39th Ward A Public Service PAC"
a2 <- "39TH WARD A PUBLIC SERVICE PAC"
w39A_names <- c(a1,a2)

b1 <- "FRIENDS OF WARD 39-B"
b2 <-"39th B Ward" 
b3 <- "Friends of Ward 39-B" 
b4 <- "Friends of Ward 39 B"
w39B_names <- c(b1, b2, b3, b4)


#ward 40 
w40_f <- dat %>% filter(grepl("\\b40th\\b", FilerName, ignore.case=TRUE) | grepl("fortieth", FilerName, ignore.case=TRUE) | 
                           grepl("\\b40\\b", FilerName)| grepl("forty", FilerName, ignore.case=TRUE))
w40_e <- dat %>% filter(grepl("\\b40th\\b", EntityName, ignore.case=TRUE) | grepl("fortieth", EntityName, ignore.case=TRUE) | 
                          grepl("\\b40\\b", EntityName)| grepl("forty", EntityName, ignore.case=TRUE))
w40_e <- w40_e %>% filter(!grepl("520-40", EntityName, ignore.case=TRUE) & 
                            !grepl("teamsters", EntityName, ignore.case=TRUE) & 
                            !grepl("worker.*", EntityName, ignore.case=TRUE))
  ## decided to include "Robert Stewart, Treasurer Ward 40-A" 

#w40_names <- unique(c(as.vector(unique(w40_f$FilerName)),as.vector(unique(w40_e$EntityName)))) 

a1 <-  "Ward 40-A Democratric Club"
a2 <- "Robert Stewart, Treasurer Ward 40-A"
w40A_names <- c(a1,a2)

b1 <- "40 B Ward Democratic Exec Com"
b2 <-"40th B Ward Committee" 
b3 <- "40 B WARD DEMOCRATIC EXEC COM"  
w40B_names <- c(b1, b2, b3)

#ward 41
w41_f <- dat %>% filter(grepl("\\b41st\\b", FilerName, ignore.case=TRUE) | grepl("fortyfirst", FilerName, ignore.case=TRUE) | 
                           grepl("forty-first", FilerName, ignore.case=TRUE)| grepl("\\b41\\b", FilerName)| 
                           grepl("forty first", FilerName, ignore.case=TRUE)| grepl("forty one", FilerName, ignore.case=TRUE)| 
                           grepl("fortyone", FilerName, ignore.case=TRUE))

w41_e <- dat %>% filter(grepl("\\b41st\\b", EntityName, ignore.case=TRUE) | grepl("fortyfirst", EntityName, ignore.case=TRUE) | 
                          grepl("forty-first", EntityName, ignore.case=TRUE)| grepl("\\b41\\b", EntityName)| 
                          grepl("forty first", EntityName, ignore.case=TRUE)| grepl("forty one", EntityName, ignore.case=TRUE)| 
                          grepl("fortyone", EntityName, ignore.case=TRUE))
w41_e <- w41_e %>% filter(!grepl("modell's", EntityName, ignore.case=TRUE))
w41_names <- unique(c(as.vector(unique(w41_f$FilerName)),as.vector(unique(w41_e$EntityName)))) 

# ward 42 
w42_f <- dat %>% filter(grepl("\\b42nd\\b", FilerName, ignore.case = TRUE)| grepl("\\b42\\b", FilerName) | 
                           grepl("fortysecond", FilerName, ignore.case=TRUE) | grepl("forty-second", FilerName, ignore.case=TRUE)| 
                           grepl("forty second", FilerName, ignore.case=TRUE)| grepl("forty two", FilerName, ignore.case=TRUE)| 
                           grepl("fortytwo", FilerName, ignore.case=TRUE))
w42_e <- dat %>% filter(grepl("\\b42nd\\b", EntityName, ignore.case = TRUE)| grepl("\\b42\\b", EntityName) | 
                          grepl("fortysecond", EntityName, ignore.case=TRUE) | grepl("forty-second", EntityName, ignore.case=TRUE)| 
                          grepl("forty second", EntityName, ignore.case=TRUE)| grepl("forty two", EntityName, ignore.case=TRUE)| 
                          grepl("fortytwo", EntityName, ignore.case=TRUE))

w42_names <- unique(c(as.vector(unique(w42_f$FilerName)),as.vector(unique(w42_e$EntityName)))) 

#ward 43 
w43_f <- dat %>% filter(grepl("\\b43rd\\b", FilerName, ignore.case = TRUE)| grepl("\\b43\\b", FilerName) | 
                           grepl("fortythird", FilerName, ignore.case=TRUE) | grepl("forty-third", FilerName, ignore.case=TRUE)| 
                           grepl("forty third", FilerName, ignore.case=TRUE)| grepl("forty three", FilerName, ignore.case=TRUE)| 
                           grepl("fortythree", FilerName, ignore.case=TRUE))
w43_e <- dat %>% filter(grepl("\\b43rd\\b", EntityName, ignore.case = TRUE)| grepl("\\b43\\b", EntityName) | 
                          grepl("fortythird", EntityName, ignore.case=TRUE) | grepl("forty-third", EntityName, ignore.case=TRUE)| 
                          grepl("forty third", EntityName, ignore.case=TRUE)| grepl("forty three", EntityName, ignore.case=TRUE)| 
                          grepl("fortythree", EntityName, ignore.case=TRUE))
w43_e <- w43_e %>% filter(!grepl("fop", EntityName, ignore.case=TRUE))
w43_names <- unique(c(as.vector(unique(w43_f$FilerName)),as.vector(unique(w43_e$EntityName)))) 

#ward 44 
w44_f <- dat %>% filter(grepl("\\b44th\\b", FilerName, ignore.case = TRUE)| grepl("\\b44\\b", FilerName) | 
                           grepl("fortyfourth", FilerName, ignore.case=TRUE) | grepl("forty-fourth", FilerName, ignore.case=TRUE)| 
                           grepl("forty fourth", FilerName, ignore.case=TRUE)| grepl("forty four", FilerName, ignore.case=TRUE)| 
                           grepl("fortyfour", FilerName, ignore.case=TRUE))
w44_e <- dat %>% filter(grepl("\\b44th\\b", EntityName, ignore.case = TRUE)| grepl("\\b44\\b", EntityName) | 
                          grepl("fortyfourth", EntityName, ignore.case=TRUE) | grepl("forty-fourth", EntityName, ignore.case=TRUE)| 
                          grepl("forty fourth", EntityName, ignore.case=TRUE)| grepl("forty four", EntityName, ignore.case=TRUE)| 
                          grepl("fortyfour", EntityName, ignore.case=TRUE))
w44_e <- w44_e %>% filter(!grepl("sheet|metal", EntityName, ignore.case=TRUE))
w44_names <- unique(c(as.vector(unique(w44_f$FilerName)),as.vector(unique(w44_e$EntityName)))) 


#ward 45 
w45_f <- dat %>% filter(grepl("\\b45th\\b", FilerName, ignore.case = TRUE)| grepl("\\b45\\b", FilerName) | 
                           grepl("fortyfifth", FilerName, ignore.case=TRUE) | grepl("forty-fifth", FilerName, ignore.case=TRUE)| 
                           grepl("forty fifth", FilerName, ignore.case=TRUE)| grepl("forty five", FilerName, ignore.case=TRUE)| 
                           grepl("fortyfive", FilerName, ignore.case=TRUE))
w45_e <- dat %>% filter(grepl("\\b45th\\b", EntityName, ignore.case = TRUE)| grepl("\\b45\\b", EntityName) | 
                          grepl("fortyfifth", EntityName, ignore.case=TRUE) | grepl("forty-fifth", EntityName, ignore.case=TRUE)| 
                          grepl("forty fifth", EntityName, ignore.case=TRUE)| grepl("forty five", EntityName, ignore.case=TRUE)| 
                          grepl("fortyfive", EntityName, ignore.case=TRUE))
w45_e <- w45_e %>% filter(!grepl("bond", EntityName, ignore.case=TRUE))
w45_names <- unique(c(as.vector(unique(w45_f$FilerName)),as.vector(unique(w45_e$EntityName)))) 


#ward 46 
w46_f <- dat %>% filter(grepl("\\b46th\\b", FilerName, ignore.case = TRUE)| grepl("\\b46\\b", FilerName) | 
                           grepl("fortysixth", FilerName, ignore.case=TRUE) | grepl("forty-sixth", FilerName, ignore.case=TRUE)| 
                           grepl("forty sixth", FilerName, ignore.case=TRUE)| grepl("forty six", FilerName, ignore.case=TRUE)| 
                           grepl("fortysix", FilerName, ignore.case=TRUE))
w46_e <- dat %>% filter(grepl("\\b46th\\b", EntityName, ignore.case = TRUE)| grepl("\\b46\\b", EntityName) | 
                          grepl("fortysixth", EntityName, ignore.case=TRUE) | grepl("forty-sixth", EntityName, ignore.case=TRUE)| 
                          grepl("forty sixth", EntityName, ignore.case=TRUE)| grepl("forty six", EntityName, ignore.case=TRUE)| 
                          grepl("fortysix", EntityName, ignore.case=TRUE))
## decided to include Jane Blackwell 46th Ward; she's a ward leader 

w46_names <- unique(c(as.vector(unique(w46_f$FilerName)),as.vector(unique(w46_e$EntityName)))) 


#ward 47 
w47_f <- dat %>% filter(grepl("\\b47th\\b", FilerName, ignore.case = TRUE)| grepl("\\b47\\b", FilerName) | 
                           grepl("fortyseventh", FilerName, ignore.case=TRUE) | grepl("forty-seventh", FilerName, ignore.case=TRUE)| 
                           grepl("forty seventh", FilerName, ignore.case=TRUE)| grepl("forty seven", FilerName, ignore.case=TRUE)| 
                           grepl("fortyseven", FilerName, ignore.case=TRUE))
w47_f <- w47_f %>% filter(grepl("\\b47th\\b", FilerName, ignore.case = TRUE))
w47_e <- dat %>% filter(grepl("\\b47th\\b", EntityName, ignore.case = TRUE)| grepl("\\b47\\b", EntityName) | 
                          grepl("fortyseventh", EntityName, ignore.case=TRUE) | grepl("forty-seventh", EntityName, ignore.case=TRUE)| 
                          grepl("forty seventh", EntityName, ignore.case=TRUE)| grepl("forty seven", EntityName, ignore.case=TRUE)| 
                          grepl("fortyseven", EntityName, ignore.case=TRUE))
w47_e <- w47_e %>% filter(grepl("\\b47th\\b", EntityName, ignore.case = TRUE))

w47_names <- unique(c(as.vector(unique(w47_f$FilerName)),as.vector(unique(w47_e$EntityName)))) 



#ward 48 
w48_f <- dat %>% filter(grepl("\\b48th\\b", FilerName, ignore.case = TRUE)| grepl("\\b48\\b", FilerName) | 
                           grepl("fortyeighth", FilerName, ignore.case=TRUE) | grepl("forty-eighth", FilerName, ignore.case=TRUE)| 
                           grepl("forty eighth", FilerName, ignore.case=TRUE)| grepl("forty eight", FilerName, ignore.case=TRUE)| 
                           grepl("fortyeight", FilerName, ignore.case=TRUE))
w48_e <- dat %>% filter(grepl("\\b48th\\b", EntityName, ignore.case = TRUE)| grepl("\\b48\\b", EntityName) | 
                          grepl("fortyeighth", EntityName, ignore.case=TRUE) | grepl("forty-eighth", EntityName, ignore.case=TRUE)| 
                          grepl("forty eighth", EntityName, ignore.case=TRUE)| grepl("forty eight", EntityName, ignore.case=TRUE)| 
                          grepl("fortyeight", EntityName, ignore.case=TRUE))
w48_e <- w48_e %>% filter(!grepl("plaza|parking", EntityName, ignore.case = TRUE))
  # included "48 Committeepeople" 
w48_names <- unique(c(as.vector(unique(w48_f$FilerName)),as.vector(unique(w48_e$EntityName)))) 


#ward 49 
w49_f <- dat %>% filter(grepl("\\b49th\\b", FilerName, ignore.case = TRUE)| grepl("\\b49\\b", FilerName) | 
                             grepl("fortyninth", FilerName, ignore.case=TRUE) | grepl("forty-ninth", FilerName, ignore.case=TRUE)| 
                             grepl("forty ninth", FilerName, ignore.case=TRUE)| grepl("forty nine", FilerName, ignore.case=TRUE)| 
                             grepl("fortynine", FilerName, ignore.case=TRUE))

w49_e <- dat %>% filter(grepl("\\b49th\\b", EntityName, ignore.case = TRUE)| grepl("\\b49\\b", EntityName) | 
                          grepl("fortyninth", EntityName, ignore.case=TRUE) | grepl("forty-ninth", EntityName, ignore.case=TRUE)| 
                          grepl("forty ninth", EntityName, ignore.case=TRUE)| grepl("forty nine", EntityName, ignore.case=TRUE)| 
                          grepl("fortynine", EntityName, ignore.case=TRUE))
w49_e <- w49_e %>% filter(!grepl("joelette", EntityName, ignore.case = TRUE))
w49_names <- unique(c(as.vector(unique(w49_f$FilerName)),as.vector(unique(w49_e$EntityName)))) 


# ward 50 
w50_f <- dat %>% filter(grepl("\\b50th\\b", FilerName, ignore.case=TRUE) | grepl("fiftieth", FilerName, ignore.case=TRUE) | 
                           grepl("\\b50\\b", FilerName)| grepl("fifty", FilerName, ignore.case=TRUE))

w50_e <- dat %>% filter(grepl("\\b50th\\b", EntityName, ignore.case=TRUE) | grepl("fiftieth", EntityName, ignore.case=TRUE) | 
                          grepl("\\b50\\b", EntityName)| grepl("fifty", EntityName, ignore.case=TRUE))
#included an entity just called "fifty" 
w50_names <- unique(c(as.vector(unique(w50_f$FilerName)),as.vector(unique(w50_e$EntityName)))) 



#ward 51
w51_f <- dat %>% filter(grepl("\\b51st\\b", FilerName, ignore.case=TRUE) | grepl("fiftyfirst", FilerName, ignore.case=TRUE) | 
                           grepl("fifty-first", FilerName, ignore.case=TRUE)| grepl("\\b51\\b", FilerName)| 
                           grepl("fifty first", FilerName, ignore.case=TRUE)| grepl("fifty one", FilerName, ignore.case=TRUE)| 
                           grepl("fiftyone", FilerName, ignore.case=TRUE))

w51_e <- dat %>% filter(grepl("\\b51st\\b", EntityName, ignore.case=TRUE) | grepl("fiftyfirst", EntityName, ignore.case=TRUE) | 
                          grepl("fifty-first", EntityName, ignore.case=TRUE)| grepl("\\b51\\b", EntityName)| 
                          grepl("fifty first", EntityName, ignore.case=TRUE)| grepl("fifty one", EntityName, ignore.case=TRUE)| 
                          grepl("fiftyone", EntityName, ignore.case=TRUE))
w51_names <- unique(c(as.vector(unique(w51_f$FilerName)),as.vector(unique(w51_e$EntityName)))) 


# ward 52 
w52_f <- dat %>% filter(grepl("\\b52nd\\b", FilerName, ignore.case = TRUE)| grepl("\\b52\\b", FilerName) | 
                           grepl("fiftysecond", FilerName, ignore.case=TRUE) | grepl("fifty-second", FilerName, ignore.case=TRUE)| 
                           grepl("fifty second", FilerName, ignore.case=TRUE)| grepl("fifty two", FilerName, ignore.case=TRUE)| 
                           grepl("fiftytwo", FilerName, ignore.case=TRUE))
w52_e <- dat %>% filter(grepl("\\b52nd\\b", EntityName, ignore.case = TRUE)| grepl("\\b52\\b", EntityName) | 
                          grepl("fiftysecond", EntityName, ignore.case=TRUE) | grepl("fifty-second", EntityName, ignore.case=TRUE)| 
                          grepl("fifty second", EntityName, ignore.case=TRUE)| grepl("fifty two", EntityName, ignore.case=TRUE)| 
                          grepl("fiftytwo", EntityName, ignore.case=TRUE))
w52_names <- unique(c(as.vector(unique(w52_f$FilerName)),as.vector(unique(w52_e$EntityName)))) 


#ward 53 
w53_f <- dat %>% filter(grepl("\\b53rd\\b", FilerName, ignore.case = TRUE)| grepl("\\b53\\b", FilerName) | 
                           grepl("fiftythird", FilerName, ignore.case=TRUE) | grepl("fifty-third", FilerName, ignore.case=TRUE)| 
                           grepl("fifty third", FilerName, ignore.case=TRUE)| grepl("fifty three", FilerName, ignore.case=TRUE)| 
                           grepl("fiftythree", FilerName, ignore.case=TRUE))
w53_f <- w53_f %>% filter(!grepl("Teamsters Joint Council No. 53 Political Action Committee - DRIVE", FilerName, ignore.case = TRUE))

w53_e <- dat %>% filter(grepl("\\b53rd\\b", EntityName, ignore.case = TRUE)| grepl("\\b53\\b", EntityName) | 
                          grepl("fiftythird", EntityName, ignore.case=TRUE) | grepl("fifty-third", EntityName, ignore.case=TRUE)| 
                          grepl("fifty third", EntityName, ignore.case=TRUE)| grepl("fifty three", EntityName, ignore.case=TRUE)| 
                          grepl("fiftythree", EntityName, ignore.case=TRUE))
w53_e <- w53_e %>% filter(!grepl("teamsters", EntityName, ignore.case = TRUE))
w53_names <- unique(c(as.vector(unique(w53_f$FilerName)),as.vector(unique(w53_e$EntityName)))) 


#ward 54 
w54_f <- dat %>% filter(grepl("\\b54th\\b", FilerName, ignore.case = TRUE)| grepl("\\b54\\b", FilerName) | 
                           grepl("fiftyfourth", FilerName, ignore.case=TRUE) | grepl("fifty-fourth", FilerName, ignore.case=TRUE)| 
                           grepl("fifty fourth", FilerName, ignore.case=TRUE)| grepl("fifty four", FilerName, ignore.case=TRUE)| 
                           grepl("fiftyfour", FilerName, ignore.case=TRUE))
w54_e <- dat %>% filter(grepl("\\b54th\\b", EntityName, ignore.case = TRUE)| grepl("\\b54\\b", EntityName) | 
                          grepl("fiftyfourth", EntityName, ignore.case=TRUE) | grepl("fifty-fourth", EntityName, ignore.case=TRUE)| 
                          grepl("fifty fourth", EntityName, ignore.case=TRUE)| grepl("fifty four", EntityName, ignore.case=TRUE)| 
                          grepl("fiftyfour", EntityName, ignore.case=TRUE))
w54_names <- unique(c(as.vector(unique(w54_f$FilerName)),as.vector(unique(w54_e$EntityName)))) 


#ward 55 
w55_f <- dat %>% filter(grepl("\\b55th\\b", FilerName, ignore.case = TRUE)| grepl("\\b55\\b", FilerName) | 
                           grepl("fiftyfifth", FilerName, ignore.case=TRUE) | grepl("fifty-fifth", FilerName, ignore.case=TRUE)| 
                           grepl("fifty fifth", FilerName, ignore.case=TRUE)| grepl("fifty five", FilerName, ignore.case=TRUE)| 
                           grepl("fiftyfive", FilerName, ignore.case=TRUE))
w55_e <- dat %>% filter(grepl("\\b55th\\b", EntityName, ignore.case = TRUE)| grepl("\\b55\\b", EntityName) | 
                          grepl("fiftyfifth", EntityName, ignore.case=TRUE) | grepl("fifty-fifth", EntityName, ignore.case=TRUE)| 
                          grepl("fifty fifth", EntityName, ignore.case=TRUE)| grepl("fifty five", EntityName, ignore.case=TRUE)| 
                          grepl("fiftyfive", EntityName, ignore.case=TRUE))
  #included "Active 55th", "Robert N. Dellavella, 55th Ward Leader" 
w55_names <- unique(c(as.vector(unique(w55_f$FilerName)),as.vector(unique(w55_e$EntityName)))) 


#ward 56 
w56_f <- dat %>% filter(grepl("\\b56th\\b", FilerName, ignore.case = TRUE)| grepl("\\b56\\b", FilerName) | 
                           grepl("fiftysixth", FilerName, ignore.case=TRUE) | grepl("fifty-sixth", FilerName, ignore.case=TRUE)| 
                           grepl("fifty sixth", FilerName, ignore.case=TRUE)| grepl("fifty six", FilerName, ignore.case=TRUE)| 
                           grepl("fiftysix", FilerName, ignore.case=TRUE))
w56_e <- dat %>% filter(grepl("\\b56th\\b", EntityName, ignore.case = TRUE)| grepl("\\b56\\b", EntityName) | 
                          grepl("fiftysixth", EntityName, ignore.case=TRUE) | grepl("fifty-sixth", EntityName, ignore.case=TRUE)| 
                          grepl("fifty sixth", EntityName, ignore.case=TRUE)| grepl("fifty six", EntityName, ignore.case=TRUE)| 
                          grepl("fiftysix", EntityName, ignore.case=TRUE))
w56_e <- w56_e %>% filter(!grepl("convention", EntityName, ignore.case = TRUE))
w56_names <- unique(c(as.vector(unique(w56_f$FilerName)),as.vector(unique(w56_e$EntityName)))) 


#ward 57 
w57_f <- dat %>% filter(grepl("\\b57th\\b", FilerName, ignore.case = TRUE)| grepl("\\b57\\b", FilerName) | 
                           grepl("fiftyseventh", FilerName, ignore.case=TRUE) | grepl("fifty-seventh", FilerName, ignore.case=TRUE)| 
                           grepl("fifty seventh", FilerName, ignore.case=TRUE)| grepl("fifty seven", FilerName, ignore.case=TRUE)| 
                           grepl("fiftyseven", FilerName, ignore.case=TRUE))
w57_e <- dat %>% filter(grepl("\\b57th\\b|\\b57\\b|fiftyseventh|fifty-seventh|fifty seventh|fifty seven|fiftyseven", EntityName, ignore.case=TRUE))
w57_e <- w57_e %>% filter(!grepl("local|district", EntityName, ignore.case = TRUE)) ## just realized that these filters could be way condensed ^ 
w57_names <- unique(c(as.vector(unique(w57_f$FilerName)),as.vector(unique(w57_e$EntityName)))) 

  # NONE 

#ward 58 
w58_f <- dat %>% filter(grepl("\\b58th\\b", FilerName, ignore.case = TRUE)| grepl("\\b58\\b", FilerName) | 
                           grepl("fiftyeighth", FilerName, ignore.case=TRUE) | grepl("fifty-eighth", FilerName, ignore.case=TRUE)| 
                           grepl("fifty eighth", FilerName, ignore.case=TRUE)| grepl("fifty eight", FilerName, ignore.case=TRUE)| 
                           grepl("fiftyeight", FilerName, ignore.case=TRUE))
w58_e <- dat %>% filter(grepl("\\b58th\\b", EntityName, ignore.case = TRUE)| grepl("\\b58\\b", EntityName) | 
                          grepl("fiftyeighth", EntityName, ignore.case=TRUE) | grepl("fifty-eighth", EntityName, ignore.case=TRUE)| 
                          grepl("fifty eighth", EntityName, ignore.case=TRUE)| grepl("fifty eight", EntityName, ignore.case=TRUE)| 
                          grepl("fiftyeight", EntityName, ignore.case=TRUE))
w58_names <- unique(c(as.vector(unique(w58_f$FilerName)),as.vector(unique(w58_e$EntityName)))) 

#ward 59 
w59_f <- dat %>% filter(grepl("\\b59th\\b", FilerName, ignore.case = TRUE)| grepl("\\b59\\b", FilerName) | 
                           grepl("fiftyninth", FilerName, ignore.case=TRUE) | grepl("fifty-ninth", FilerName, ignore.case=TRUE)| 
                           grepl("fifty ninth", FilerName, ignore.case=TRUE)| grepl("fifty nine", FilerName, ignore.case=TRUE)| 
                           grepl("fiftynine", FilerName, ignore.case=TRUE))
w59_e <- dat %>% filter(grepl("\\b59th\\b", EntityName, ignore.case = TRUE)| grepl("\\b59\\b", EntityName) | 
                          grepl("fiftyninth", EntityName, ignore.case=TRUE) | grepl("fifty-ninth", EntityName, ignore.case=TRUE)| 
                          grepl("fifty ninth", EntityName, ignore.case=TRUE)| grepl("fifty nine", EntityName, ignore.case=TRUE)| 
                          grepl("fiftynine", EntityName, ignore.case=TRUE))
w59_names <- unique(c(as.vector(unique(w59_f$FilerName)),as.vector(unique(w59_e$EntityName)))) 

# ward 60 
 
w60_f <- dat %>% filter(grepl("\\b60th\\b", FilerName, ignore.case=TRUE) | grepl("sixtieth", FilerName, ignore.case=TRUE) | 
                           grepl("\\b60\\b", FilerName)| grepl("sixty", FilerName, ignore.case=TRUE))
w60_e <- dat %>% filter(grepl("\\b60th\\b", EntityName, ignore.case=TRUE) | grepl("sixtieth", EntityName, ignore.case=TRUE) | 
                          grepl("\\b60\\b", EntityName)| grepl("sixty", EntityName, ignore.case=TRUE))
w60_names <- unique(c(as.vector(unique(w60_f$FilerName)),as.vector(unique(w60_e$EntityName)))) 

#ward 61
w61_f <- dat %>% filter(grepl("\\b61st\\b", FilerName, ignore.case=TRUE) | grepl("sixtyfirst", FilerName, ignore.case=TRUE) | 
                           grepl("sixty-first", FilerName, ignore.case=TRUE)| grepl("\\b61\\b", FilerName)| 
                           grepl("sixty first", FilerName, ignore.case=TRUE)| grepl("sixty one", FilerName, ignore.case=TRUE)| 
                           grepl("sixtyone", FilerName, ignore.case=TRUE))

w61_e <- dat %>% filter(grepl("\\b61st\\b", EntityName, ignore.case=TRUE) | grepl("sixtyfirst", EntityName, ignore.case=TRUE) | 
                          grepl("sixty-first", EntityName, ignore.case=TRUE)| grepl("\\b61\\b", EntityName)| 
                          grepl("sixty first", EntityName, ignore.case=TRUE)| grepl("sixty one", EntityName, ignore.case=TRUE)| 
                          grepl("sixtyone", EntityName, ignore.case=TRUE))
w61_e <- w61_e %>% filter(!grepl("lot", EntityName, ignore.case = TRUE)) 
w61_names <- unique(c(as.vector(unique(w61_f$FilerName)),as.vector(unique(w61_e$EntityName)))) 

# ward 62 
w62_f <- dat %>% filter(grepl("\\b62nd\\b", FilerName, ignore.case = TRUE)| grepl("\\b62\\b", FilerName) | 
                           grepl("sixtysecond", FilerName, ignore.case=TRUE) | grepl("sixty-second", FilerName, ignore.case=TRUE)| 
                           grepl("sixty second", FilerName, ignore.case=TRUE)| grepl("sixty two", FilerName, ignore.case=TRUE)| 
                           grepl("sixtytwo", FilerName, ignore.case=TRUE))
w62_e <- dat %>% filter(grepl("\\b62nd\\b", EntityName, ignore.case = TRUE)| grepl("\\b62\\b", EntityName) | 
                          grepl("sixtysecond", EntityName, ignore.case=TRUE) | grepl("sixty-second", EntityName, ignore.case=TRUE)| 
                          grepl("sixty second", EntityName, ignore.case=TRUE)| grepl("sixty two", EntityName, ignore.case=TRUE)| 
                          grepl("sixtytwo", EntityName, ignore.case=TRUE))
w62_e <- w62_e %>% filter(!grepl("high", EntityName, ignore.case = TRUE)) 
w62_names <- unique(c(as.vector(unique(w62_f$FilerName)),as.vector(unique(w62_e$EntityName)))) 


#ward 63 
w63_f <- dat %>% filter(grepl("\\b63rd\\b", FilerName, ignore.case = TRUE)| grepl("\\b63\\b", FilerName) | 
                           grepl("sixtythird", FilerName, ignore.case=TRUE) | grepl("sixty-third", FilerName, ignore.case=TRUE)| 
                           grepl("sixty third", FilerName, ignore.case=TRUE)| grepl("sixty three", FilerName, ignore.case=TRUE)| 
                           grepl("sixtythree", FilerName, ignore.case=TRUE))
w63_e <- dat %>% filter(grepl("\\b63rd\\b", EntityName, ignore.case = TRUE)| grepl("\\b63\\b", EntityName) | 
                          grepl("sixtythird", EntityName, ignore.case=TRUE) | grepl("sixty-third", EntityName, ignore.case=TRUE)| 
                          grepl("sixty third", EntityName, ignore.case=TRUE)| grepl("sixty three", EntityName, ignore.case=TRUE)| 
                          grepl("sixtythree", EntityName, ignore.case=TRUE))
w63_names <- unique(c(as.vector(unique(w63_f$FilerName)),as.vector(unique(w63_e$EntityName)))) 

#ward 64 
w64_f <- dat %>% filter(grepl("\\b64th\\b", FilerName, ignore.case = TRUE)| grepl("\\b64\\b", FilerName) | 
                           grepl("sixtyfourth", FilerName, ignore.case=TRUE) | grepl("sixty-fourth", FilerName, ignore.case=TRUE)| 
                           grepl("sixty fourth", FilerName, ignore.case=TRUE)| grepl("sixty four", FilerName, ignore.case=TRUE)| 
                           grepl("sixtyfour", FilerName, ignore.case=TRUE))
w64_e <- dat %>% filter(grepl("\\b64th\\b", EntityName, ignore.case = TRUE)| grepl("\\b64\\b", EntityName) | 
                          grepl("sixtyfourth", EntityName, ignore.case=TRUE) | grepl("sixty-fourth", EntityName, ignore.case=TRUE)| 
                          grepl("sixty fourth", EntityName, ignore.case=TRUE)| grepl("sixty four", EntityName, ignore.case=TRUE)| 
                          grepl("sixtyfour", EntityName, ignore.case=TRUE))
w64_names <- unique(c(as.vector(unique(w64_f$FilerName)),as.vector(unique(w64_e$EntityName)))) 


#ward 65 
w65_f <- dat %>% filter(grepl("\\b65th\\b", FilerName, ignore.case = TRUE)| grepl("\\b65\\b", FilerName) | 
                           grepl("sixtyfifth", FilerName, ignore.case=TRUE) | grepl("sixty-fifth", FilerName, ignore.case=TRUE)| 
                           grepl("sixty fifth", FilerName, ignore.case=TRUE)| grepl("sixty five", FilerName, ignore.case=TRUE)| 
                           grepl("sixtyfive", FilerName, ignore.case=TRUE))
w65_e <- dat %>% filter(grepl("\\b65th\\b", EntityName, ignore.case = TRUE)| grepl("\\b65\\b", EntityName) | 
                          grepl("sixtyfifth", EntityName, ignore.case=TRUE) | grepl("sixty-fifth", EntityName, ignore.case=TRUE)| 
                          grepl("sixty fifth", EntityName, ignore.case=TRUE)| grepl("sixty five", EntityName, ignore.case=TRUE)| 
                          grepl("sixtyfive", EntityName, ignore.case=TRUE))
w65_e <- w65_e %>% filter(!grepl("matthew", EntityName, ignore.case = TRUE)) 
w65_names <- unique(c(as.vector(unique(w65_f$FilerName)),as.vector(unique(w65_e$EntityName)))) 


#ward 66 
w66_f <- dat %>% filter(grepl("\\b66th\\b", FilerName, ignore.case = TRUE)| grepl("\\b66\\b", FilerName) | 
                           grepl("sixtysixth", FilerName, ignore.case=TRUE) | grepl("sixty-sixth", FilerName, ignore.case=TRUE)| 
                           grepl("sixty sixth", FilerName, ignore.case=TRUE)| grepl("sixty six", FilerName, ignore.case=TRUE)| 
                           grepl("sixtysix", FilerName, ignore.case=TRUE))
w66_e <- dat %>% filter(grepl("\\b66th\\b", EntityName, ignore.case = TRUE)| grepl("\\b66\\b", EntityName) | 
                          grepl("sixtysixth", EntityName, ignore.case=TRUE) | grepl("sixty-sixth", EntityName, ignore.case=TRUE)| 
                          grepl("sixty sixth", EntityName, ignore.case=TRUE)| grepl("sixty six", EntityName, ignore.case=TRUE)| 
                          grepl("sixtysix", EntityName, ignore.case=TRUE))
# w66_names <- unique(c(as.vector(unique(w66_f$FilerName)),as.vector(unique(w66_e$EntityName)))) 
a1 <-  "Ward 66-A, Friends of the"
a2 <- "Friends of 66 A Ward"
w66A_names <- c(a1,a2)

b1 <- "Friends of 66th B"
b2 <- "Friends of 66"
b3 <- "FRIENDS OF 66"
w66B_names <- c(b1)

x1 <- "66th Ward GOP" #We're not analyzing republican party finance anyways, this would get removed 
x2 <- "Friends of 66" # B (address is 12410 Tyrone Rd)
x3 <-  "FRIENDS OF 66" # B (address is 12410 Tyrone Rd)
x4 <- "66 Ward GOP" #We're not analyzing republican party finance anyways, this would get removed 
# w66x_names <- c(x1, x2, x3, x4)
 
# candidate grouping ----------------

## CANDIDATES : create master list of filer names for the candidates 

#larry krasner 
lk_f <- dat %>% filter(grepl("krasner|larry krasner|lawrence krasner", FilerName, ignore.case = TRUE))
lk_e <- dat %>% filter(grepl("krasner|larry krasner|lawrence krasner", EntityName, ignore.case = TRUE))
lk_names <- unique(c(as.vector(unique(lk_f$FilerName)),as.vector(unique(lk_e$EntityName)))) 
lk_names <- lk_names[!grepl("Rebecca Krasner", lk_names, ignore.case=TRUE  )] 
  # rebecca is not his wife, and should be excluded from krasner related filings 


# teresa carr deni
td_f <- dat %>% filter(grepl("carr deni|teresa carr deni|Deni for DA", FilerName, ignore.case = TRUE))
td_e <- dat %>% filter(grepl("carr deni|teresa carr deni|Deni for DA", EntityName, ignore.case = TRUE))

td_names <- unique(c(as.vector(unique(td_f$FilerName)),as.vector(unique(td_e$EntityName)))) 
 


# tariq el-shabazz
te_f <- dat %>% filter(grepl("tariq|el-shabazz|el shabazz", FilerName, ignore.case = TRUE))
te_e <- dat %>% filter(grepl("tariq|el-shabazz|el shabazz", EntityName, ignore.case = TRUE))
te_names <- unique(c(as.vector(unique(te_f$FilerName)),as.vector(unique(te_e$EntityName)))) 
te_names <- te_names[!grepl("abdulmalik|lear", te_names, ignore.case =TRUE )] # filter a list 

# joseph J. "joe" khan 
jk_f <- dat %>% filter(grepl("joe khan|joseph khan", FilerName, ignore.case = TRUE)) #none showed up as Kahn 
jk_e <- dat %>% filter(grepl("joe khan|joseph khan", EntityName, ignore.case = TRUE))

jk_names <- unique(c(as.vector(unique(jk_f$FilerName)),as.vector(unique(jk_e$EntityName)))) 


# richard "rich" negrin
rn_f <- dat %>% filter(grepl("rich negrin|negrin", FilerName, ignore.case = TRUE))
rn_e <- dat %>% filter(grepl("rich negrin|negrin", EntityName, ignore.case = TRUE))
rn_names <- unique(c(as.vector(unique(rn_f$FilerName)),as.vector(unique(rn_e$EntityName)))) 
rn_names <- rn_names[!grepl("karen", rn_names, ignore.case= TRUE  )]  
## karen McRory-Negrin is is wife -- I'm going to exclude her even though they might share finances 


#john "jack" o'neill ### find other oneill spellings !!! 
jo_f <- dat %>% filter(grepl("jack o'neill|john o'neill|jack o neill|john o neill|jack oneill|john oneill", FilerName, ignore.case = TRUE))
jo_e <- dat %>% filter(grepl("jack o'neill|john o'neill|jack o neill|john o neill|jack oneill|john oneill", EntityName, ignore.case = TRUE))
jo_names <- unique(c(as.vector(unique(jo_f$FilerName)),as.vector(unique(jo_e$EntityName)))) 

#micheal untermeyer
mu_f <- dat %>% filter(grepl("untermeyer", FilerName, ignore.case = TRUE))
mu_e <- dat %>% filter(grepl("untermeyer", EntityName, ignore.case = TRUE))
mu_names <- unique(c(as.vector(unique(mu_f$FilerName)),as.vector(unique(mu_e$EntityName)))) 




# create new columns for ward/cand entity or filers ---------------
## MAKE NEW COLUMNS
dat_clean <- dat
# create 4 mutate statements for each of the columns we want : ward_filer, ward_ent, cand_filer, cand_ent

# create and populate "ward_ent" column for transactions that have a ward as the EntityName
dat_clean <- dat_clean %>% mutate(ward_ent = case_when( 
  EntityName %in% w1_names ~ '1',              
  EntityName %in% w2_names ~ '2',
  EntityName %in% w3_names ~ '3',
  EntityName %in% w4_names ~ '4',
  EntityName %in% w5_names ~ '5',
  EntityName %in% w6_names ~ '6',
  EntityName %in% w7_names ~ '7',
  EntityName %in% w8_names ~ '8',
  EntityName %in% w9_names ~ '9',
  EntityName %in% w10_names ~ '10',
  EntityName %in% w11_names ~ '11',
  EntityName %in% w12_names ~ '12',
  EntityName %in% w13_names ~ '13',
  EntityName %in% w14_names ~ '14',
  EntityName %in% w15_names ~ '15',
  EntityName %in% w16_names ~ '16',
  EntityName %in% w17_names ~ '17',
  EntityName %in% w18_names ~ '18',
  EntityName %in% w19_names ~ '19',
  EntityName %in% w20_names ~ '20',
  EntityName %in% w21_names ~ '21',
  EntityName %in% w22_names ~ '22',
  EntityName %in% w23_names ~ '23',
  EntityName %in% w24_names ~ '24',
  EntityName %in% w25_names ~ '25',
  EntityName %in% w26_names ~ '26',
  EntityName %in% w27_names ~ '27',
  EntityName %in% w28_names ~ '28',
  EntityName %in% w29_names ~ '29',
  EntityName %in% w30_names ~ '30',
  EntityName %in% w31_names ~ '31',
  EntityName %in% w32_names ~ '32',
  EntityName %in% w33_names ~ '33',
  EntityName %in% w34_names ~ '34',
  EntityName %in% w35_names ~ '35',
  EntityName %in% w36_names ~ '36',
  EntityName %in% w37_names ~ '37',
  EntityName %in% w38_names ~ '38',
  #EntityName %in% w39_names ~ '39',
  
  EntityName %in% w39A_names ~ '39A',
  EntityName %in% w39B_names ~ '39B',
  
  EntityName %in% w40B_names ~ '40A',
  EntityName %in% w40A_names ~ '40B',
  
  #EntityName %in% w40_names ~ '40',
  EntityName %in% w41_names ~ '41',
  EntityName %in% w42_names ~ '42',
  EntityName %in% w43_names ~ '43',
  EntityName %in% w44_names ~ '44',
  EntityName %in% w45_names ~ '45',
  EntityName %in% w46_names ~ '46',
  EntityName %in% w47_names ~ '47',
  EntityName %in% w48_names ~ '48',
  EntityName %in% w49_names ~ '49',
  EntityName %in% w50_names ~ '50',
  EntityName %in% w51_names ~ '51',
  EntityName %in% w52_names ~ '52',
  EntityName %in% w53_names ~ '53',
  EntityName %in% w54_names ~ '54',
  EntityName %in% w55_names ~ '55',
  EntityName %in% w56_names ~ '56',
  EntityName %in% w57_names ~ '57',
  EntityName %in% w58_names ~ '58',
  EntityName %in% w59_names ~ '59',
  EntityName %in% w60_names ~ '60',
  EntityName %in% w61_names ~ '61',
  EntityName %in% w62_names ~ '62',
  EntityName %in% w63_names ~ '63',
  EntityName %in% w64_names ~ '64',
  EntityName %in% w65_names ~ '65',
  # EntityName %in% w66_names ~ '66' )) 
  EntityName %in% w66A_names ~ '66A',
  EntityName %in% w66B_names ~ '66B')) ## note that some of 66 won't be captured because it's not designated to A or B 

# create and populate "ward_filer" column
dat_clean <- dat_clean %>% mutate(ward_filer = case_when(
  FilerName %in% w1_names ~ '1',
  FilerName %in% w2_names ~ '2',
  FilerName %in% w3_names ~ '3',
  FilerName %in% w4_names ~ '4',
  FilerName %in% w5_names ~ '5',
  FilerName %in% w6_names ~ '6',
  FilerName %in% w7_names ~ '7',
  FilerName %in% w8_names ~ '8',
  FilerName %in% w9_names ~ '9',
  FilerName %in% w10_names ~ '10',
  FilerName %in% w11_names ~ '11',
  FilerName %in% w12_names ~ '12',
  FilerName %in% w13_names ~ '13',
  FilerName %in% w14_names ~ '14',
  FilerName %in% w15_names ~ '15',
  FilerName %in% w16_names ~ '16',
  FilerName %in% w17_names ~ '17',
  FilerName %in% w18_names ~ '18',
  FilerName %in% w19_names ~ '19',
  FilerName %in% w20_names ~ '20',
  FilerName %in% w21_names ~ '21',
  FilerName %in% w22_names ~ '22',
  FilerName %in% w23_names ~ '23',
  FilerName %in% w24_names ~ '24',
  FilerName %in% w25_names ~ '25',
  FilerName %in% w26_names ~ '26',
  FilerName %in% w27_names ~ '27',
  FilerName %in% w28_names ~ '28',
  FilerName %in% w29_names ~ '29',
  FilerName %in% w30_names ~ '30',
  FilerName %in% w31_names ~ '31',
  FilerName %in% w32_names ~ '32',
  FilerName %in% w33_names ~ '33',
  FilerName %in% w34_names ~ '34',
  FilerName %in% w35_names ~ '35',
  FilerName %in% w36_names ~ '36',
  FilerName %in% w37_names ~ '37',
  FilerName %in% w38_names ~ '38',
  #FilerName %in% w39_names ~ '39',
  
  FilerName %in% w39A_names ~ '39A',
  FilerName %in% w39B_names ~ '39B',
  
  FilerName %in% w40B_names ~ '40A',
  FilerName %in% w40A_names ~ '40B',
  
  #FilerName %in% w40_names ~ '40',
  FilerName %in% w41_names ~ '41',
  FilerName %in% w42_names ~ '42',
  FilerName %in% w43_names ~ '43',
  FilerName %in% w44_names ~ '44',
  FilerName %in% w45_names ~ '45',
  FilerName %in% w46_names ~ '46',
  FilerName %in% w47_names ~ '47',
  FilerName %in% w48_names ~ '48',
  FilerName %in% w49_names ~ '49',
  FilerName %in% w50_names ~ '50',
  FilerName %in% w51_names ~ '51',
  FilerName %in% w52_names ~ '52',
  FilerName %in% w53_names ~ '53',
  FilerName %in% w54_names ~ '54',
  FilerName %in% w55_names ~ '55',
  FilerName %in% w56_names ~ '56',
  FilerName %in% w57_names ~ '57',
  FilerName %in% w58_names ~ '58',
  FilerName %in% w59_names ~ '59',
  FilerName %in% w60_names ~ '60',
  FilerName %in% w61_names ~ '61',
  FilerName %in% w62_names ~ '62',
  FilerName %in% w63_names ~ '63',
  FilerName %in% w64_names ~ '64',
  FilerName %in% w65_names ~ '65',
  #FilerName %in% w66_names ~ '66' )) 
  FilerName %in% w66A_names ~ '66A',
  FilerName %in% w66B_names ~ '66B')) ## note that some of 66 won't be captured because it's not designated to A or B 
  

#create and populate a cand_filer column
 
 dat_clean <- dat_clean %>% mutate(cand_filer = case_when(
   FilerName %in% lk_names ~ 'larry', 
   FilerName %in% td_names ~ 'teresa', 
   FilerName %in% te_names ~ 'tariq',
   FilerName %in% jk_names ~ 'joe',
   FilerName %in% rn_names ~ 'negrin',
   FilerName %in% jo_names ~ 'oneill',
   FilerName %in% mu_names ~ 'untermeyer' ))
 
 
 #create and populate a cand_ent column
 dat_clean <- dat_clean %>% mutate(cand_ent = case_when(
   EntityName %in% lk_names ~ 'larry', 
   EntityName %in% td_names ~ 'teresa', 
   EntityName %in% te_names ~ 'tariq',
   EntityName %in% jk_names ~ 'joe',
   EntityName %in% rn_names ~ 'negrin',
   EntityName %in% jo_names ~ 'oneill',
   EntityName %in% mu_names ~ 'untermeyer' ))
 
write.csv(dat_clean, 'output/campaign_finance_clean.csv',row.names = FALSE)

# list of names wards or candidates filed under ------------ 
#this part of the script was used to figure out if a filer or entity is Democratic or Repulican 
#in order to limit the finace data to Democratic transactions for the scope of the analysis 
 
###########################  list of all names wards filed under, separated by filer/entity  
## this section of the script was used to create a dataset that would denote if the name used was as filer or entity 
 list_entity_ward_names <- rbind(w1_e, w2_e, w3_e, w4_e, w5_e, w6_e, w7_e, w8_e, w9_e, w10_e,
                                w11_e, w12_e, w13_e, w14_e, w15_e, w16_e, w17_e, w18_e, w19_e, w20_e,
                                w21_e, w22_e, w23_e, w24_e, w25_e, w26_e, w27_e, w28_e, w29_e, w30_e,
                                w31_e, w32_e, w33_e, w34_e, w35_e, w36_e, w37_e, w38_e, w39_e, w40_e,
                                w41_e, w42_e, w43_e, w44_e, w45_e, w46_e, w47_e, w48_e, w49_e, w50_e,
                                w51_e, w52_e, w53_e, w54_e, w55_e, w56_e, w57_e, w58_e, w59_e, w60_e,
                                w61_e, w62_e, w63_e, w64_e, w65_e, w66_e)
 
 list_filer_ward_names <- rbind(w1_f, w2_f, w3_f, w4_f, w5_f, w6_f, w7_f, w8_f, w9_f, w10_f,
                                 w11_f, w12_f, w13_f, w14_f, w15_f, w16_f, w17_f, w18_f, w19_f, w20_f,
                                 w21_f, w22_f, w23_f, w24_f, w25_f, w26_f, w27_f, w28_f, w29_f, w30_f,
                                 w31_f, w32_f, w33_f, w34_f, w35_f, w36_f, w37_f, w38_f, w39_f, w40_f,
                                 w41_f, w42_f, w43_f, w44_f, w45_f, w46_f, w47_f, w48_f, w49_f, w50_f,
                                 w51_f, w52_f, w53_f, w54_f, w55_f, w56_f, w57_f, w58_f, w59_f, w60_f,
                                 w61_f, w62_f, w63_f, w64_f, w65_f, w66_f)
 
 list_entity_ward_names <- unique(list_entity_ward_names$EntityName)
 list_entity_ward_names <- as.data.frame(list_entity_ward_names)
 list_entity_ward_names <- list_entity_ward_names %>% mutate(category = "entity")
 colnames(list_entity_ward_names)[colnames(list_entity_ward_names)=="list_entity_ward_names"] <- "name"
 
 
 list_filer_ward_names <- unique(list_filer_ward_names$FilerName)
 list_filer_ward_names <- as.data.frame(list_filer_ward_names)
 list_filer_ward_names <- list_filer_ward_names %>% mutate(category = "filer")
 colnames(list_filer_ward_names)[colnames(list_filer_ward_names)=="list_filer_ward_names"] <- "name"
 
 list_ward_names <- rbind(list_entity_ward_names, list_filer_ward_names)
 list_ward_names = list_ward_names[!duplicated(list_ward_names$name),]

 write.csv(list_ward_names, 'output/campaign_finance_ward_filers_clean.csv', row.names = FALSE)
 
 
 