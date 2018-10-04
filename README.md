# Project Title: Cleaning Philly YTD campaign finance data 
Campaign finance data is really interesting and important for ensuring fair and transparent elections, but in Philadelphia, it’s formatted in a way that makes it near impossible to analyze without a fair bit of processing. While available on the city’s website, the data on campaign-related transactions is unstandardized and messy. This R project will allow anyone who’s interested in ward level campaign finance in Philly to clean the data so that it can be analyzed and visualized. This project filters on Year To Date files for the primary election season of 2017, and focuses on candidates of the Democratic primary for District Attorney (Larry Krasner, Rich Negrin, Joe Khan, Michael Untermeyer, etc.)

## Getting Started
Clone this repo in its entirety as each script's outputs feed into other scripts. Inside of each folder are scripts and R projects. Open the R projects in your directory in R studio.

### Prerequisites
What things you need to install the software and how to install them: 

* RStudio version 3.5.1
* Packages: 
  * dplyr, ggplot2, ggthemes, scales (built under 3.5.1) 

### Installing
First, clone the repo. 

## Running the scripts
The projects must be run in this order:
1. campaign_finance_proj.
2. census_data_proj.
3. voter_turnout_proj.
4. combine_data_sets_proj.

### campaign_finance_proj. 
Once inside this project, run these scripts in order: 

1. Campaign_finance_data_cleaning
  * *Description:* takes in the YTD file and groups filers and entities into ward categories and candidate categories. Outputs campaign_finance_clean.csv and campaign_finance_ward_filers_clean. The latter was used to parse through filers and entities and determine if they were Democrat or Republican 
  * *Input:* explorer_transactions_2017.csv, explorer_transactions_2016.csv 
  * *Output:* campaign_finance_clean.csv, campaign_finance_ward_filers_clean.csv
  
2. Amended_filtering_workspace
  * *Description:* takes in campaign_finance_clean and creates column for keep or duplicate based on amendments to the campaign finance records. output is dat_clean_with_dups_indicated.csv
  * *Input:* campaign_finance_clean.csv
  * *Output:* dat_clean_with_dups_indicated.csv

3. Campaign_finance_summarytables
  * *Description:* takes in dat_clean_with_dups_indicated and creates column for Dem or Rep. Creates summary tables for Democratic records
  * *Input:* dat_clean_with_dups_indicated,csv, campaign_finance_ward_dem_rep_noted.csv.csv
  * *Output:* campaign_finance_by_ward.csv (this is the comprehensive table-- there are also individual tables outputted: 
ward_spent, ward_rec_not_from_cand, ward_spent_gotv, ward_rec_cand, ward_rec_total, cand_rec, cand_spent_wards) 

4. Campaign_finance_visualizations
  * *Description:* creates bar charts of various summary tables 
  * *Input:* summary tables from the previous script 
  * *Output:* figures (bar charts of campaign finance) 

### census_data_proj. 
Once inside this project, run this script:

1. Census_data_cleaning
  * *Description:* Uses a file created in Arcmap (using the Union tool) to aggregate census tracts to ward boundaries based on the percentage of the tract that falls inside a ward boundary
  * *Input:* C70_ACS_download_formatted.csv, aggregation_ratio_table_withAB.csv
  * *Output:* census_var_by_ward.csv

### voter_turnout_proj. 
Once inside this project, run this script:

1. Voter_turnout_and_election_results
  * *Description:* input election results and registered voter data, output is voter turnout by ward and percentage of the vote won by each candidate at the ward level
  * *Input:* 2017_primary.csv, 2017_voter_file.csv
  * *Output:* perc_of_vote_by_ward_wide.csv, vot_turnout_17.csv, and vot_turnout_and_cand_pvote.csv

### combine_data_sets_proj.
Once inside this project, run this script:

1. combine_data_sets
  * *Description:* Combines campus finance, voter turnout, election results, and census data
  * *Inputs:* campaign_finance_by_ward, census_var_by_ward, vot_turnout_and_cand_pvote
  * *Outputs:* data_by_ward.csv

## Authors
* Gabrielle Beaudry, Azavea Summer of Maps Fellow 
* Mentor: Esther Needham, Azavea Data Analytics Project Manager 

## Acknowledgments
* This code was written as part of an Azavea Summer of Maps Fellowship. The project was for Committee of Seventy, an independent, nonpartisan advocate for better government. 
* Brian McHale, who works at the Board of Ethics and was instrumental in learning how the dataset was set up
