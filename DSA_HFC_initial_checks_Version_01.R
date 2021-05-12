### Project:  DSA High Frequency Checks
### Input:    raw_data.csv
###         
### Output:   dsa_survey_tracking.xlsx
###           dsa_issues_log.xlsx
###           dsa_enumerator_summary.xlsx
###           dsa_hfc_duplicates.xlsx
###           dsa_bc_diffs.xlsx
###           dsa_hfc_research.xlsx
###           dsa_replacement_log.xlsx
###         
### Last Date Updated: 28 November, 2019

### Install Packages
#install.packages("dplyr")
library(dplyr)
library(outliers)
library(knitr)
library(plotly)
library(stringi)
library(webshot)
library(sp)
library(rgdal)
library(data.table)
library(sqldf)
library(xlsx)
#install.packages("xlsx")

# Set Up
rm(list=ls())
initial.dir<-getwd()
setwd("C:/Users/Ahmed Mohamud/Desktop/DSA/R_script")

### Set Current Date and Start Date of Data Collection
date_current <- as.Date("2019-03-14")
date_dc_start <- as.Date("2019-02-03")


# Upload Raw Data
data <- read.csv("inputs/dsa_final.csv",stringsAsFactors=FALSE)

# Cleaning
names(data)[names(data) == 'health_facilities_others'] <- 'health_facilities_other'
names(data)[names(data) == 'health_services_others'] <- 'health_services_other'
names(data)[names(data) == 'health_problems_others'] <- 'health_problems_other'
names(data)[names(data) == 'aap_informationsources.aap_informationsources_9'] <- 'aap_informationsources.other'

data$start_date_time <- strptime(data$start, format='%d/%m/%Y %H:%M')
data$end_date_time = strptime(data$end,format='%d/%m/%Y %H:%M')
data$start_date <- as.Date(data$start_date_time)
data$end_date <- as.Date(data$end_date_time)
data$survey_duration <- strptime(data$end, '%d/%m/%Y %H:%M') - strptime(data$start, '%d/%m/%Y %H:%M')

# High Frequency Checks
dsa_hfc_issues_log <- data.frame(matrix(ncol = 9, nrow = 0))
x <- c("UUID", "Region", "District", "Settlement", "EnumeratorName", "Indicator", "Comment", "OldValue", "NewValue")
colnames(dsa_hfc_issues_log) <- x

# 1.0   Survey Tracking
# 1.1   Check the progress towards productivity/recruitment goals by day and by geographic variable
dsa_hfc_survey_tracking <- summarise(group_by(data, localisation_region, localisation_district),count =n())
list_check_survey_tracking <- unique(na.omit(data$today))
for(j in list_check_survey_tracking){
  dsa_hfc_survey_tracking[, ncol(dsa_hfc_survey_tracking) + 1] <- (data %>%
                                                                     group_by(localisation_region, localisation_district) %>%
                                                                     summarise(Val=sum(na.omit(today==j))) %>%
                                                                     arrange(localisation_region, localisation_district)) $Val 
  names(dsa_hfc_survey_tracking)[ncol(dsa_hfc_survey_tracking)] <- paste0("Date: ", j)
}

names(dsa_hfc_survey_tracking)[names(dsa_hfc_survey_tracking) == 'localisation_region'] <- 'Region'
names(dsa_hfc_survey_tracking)[names(dsa_hfc_survey_tracking) == 'localisation_district'] <- 'District'
names(dsa_hfc_survey_tracking)[names(dsa_hfc_survey_tracking) == 'count'] <- 'TotalNumberOfSurveys'

# 2.0     Issues Log
# 2.0     Logic Checks
# 2.01    Check that all submissions are using the most recent version of the survey form
data$hfc_logic_check_indicator_01<-as.integer(0)
data$hfc_logic_check_indicator_01[data$X__version__!="vXYYFiAj6JH7pRfaoWJUnV"]<-as.integer(1)
data$hfc_logic_check_comment_01<-as.character("Check that the enumerator is using the most recent version of the survey form.")
data$hfc_logic_check_indicator_name_01<-as.character("Survey Version Check")
data$hfc_logic_check_oldvalue_01 <- as.character("Not Applicable")
data$hfc_logic_check_newvalue_01 <- as.character("Not Applicable")

# 2.02    Check that all interviews were completed
data$hfc_logic_check_indicator_02<-as.integer(0)
data$hfc_logic_check_indicator_02[data$ki_referral_resident!=as.character(NA)]<-as.integer(1)
data$hfc_logic_check_comment_02<-as.character("Check that the enumerator completed the interview.")
data$hfc_logic_check_indicator_name_02<-as.character("Completion Check")
data$hfc_logic_check_oldvalue_02 <- as.character("Not Applicable")
data$hfc_logic_check_newvalue_02 <- as.character("Not Applicable")

# 2.03    Check that there are no duplicate observations
sum(table(data$X_uuid)-1)
data$hfc_logic_check_indicator_03<-as.integer(0)
data$hfc_logic_check_03<-duplicated(data$X_uuid) 
data$hfc_logic_check_indicator_03[data$hfc_logic_check_03==TRUE]<-as.integer(1)
data$hfc_logic_check_comment_03<-as.character("Check that survey is not a duplicate.")
data$hfc_logic_check_indicator_name_03<-as.character("Duplicate Survey Check")
data$hfc_logic_check_oldvalue_03 <- as.character("Not Applicable")
data$hfc_logic_check_newvalue_03 <- as.character("Not Applicable")

# 2.04    Check that all surveys have consent
data$hfc_logic_check_indicator_04<-as.integer(0)
data$hfc_logic_check_indicator_04[data$consent!="yes"]<-as.integer(1)
data$hfc_logic_check_comment_04<-as.character("Check that respondent provided consent.")
data$hfc_logic_check_indicator_name_04<-as.character("Duplicate Survey Check")
data$hfc_logic_check_oldvalue_04 <- as.character("Not Applicable")
data$hfc_logic_check_newvalue_04 <- as.character("Not Applicable")

# 2.05    Check that certain critical variables have no missing values
# NOT REQUIRED
# 2.06    Check that follow up record information matches original
# NOT REQUIRED

# 2.07    Check skip patterns and constraints
# TO BE DETERMINED

# 2.08    Check that no variable has all missing values
# NOT REQUIRED

list_indicators_logic_check <- c("01",
                                 "02",
                                 "03",
                                 "04")

for (i in list_indicators_logic_check){
  temp_name <- paste0("hfc_logic_check_", i)
  temp_name_indicator <- paste0("hfc_logic_check_indicator_", i)
  temp_name_indicator_name <- paste0("hfc_logic_check_indicator_name_", i)
  temp_name_comment <- paste0("hfc_logic_check_comment_", i)
  temp_name_oldvalue <- paste0("hfc_logic_check_oldvalue_", i)
  temp_name_newvalue <- paste0("hfc_logic_check_newvalue_", i)
  temp_name <- data.frame(data$X_uuid, data$localisation_region, data$localisation_district, data$localisation_settlement_name_local, data$deviceid, data[temp_name_indicator], data[temp_name_indicator_name], data[temp_name_comment], data[temp_name_oldvalue], data[temp_name_newvalue])
  temp_name <- subset(temp_name, data[temp_name_indicator]!=0)
  names(temp_name)[names(temp_name) == 'data.X_uuid'] <- 'UUID'
  names(temp_name)[names(temp_name) == 'data.localisation_region'] <- 'Region'
  names(temp_name)[names(temp_name) == 'data.localisation_district'] <- 'District'
  names(temp_name)[names(temp_name) == 'data.localisation_settlement_name_local'] <- 'Settlement'
  names(temp_name)[names(temp_name) == 'data.deviceid'] <- 'Enumerator'
  names(temp_name)[names(temp_name) == paste0("hfc_logic_check_indicator_name_", i)] <- 'Indicator'
  names(temp_name)[names(temp_name) == paste0("hfc_logic_check_comment_", i)] <- 'Comment'
  names(temp_name)[names(temp_name) == paste0("hfc_logic_check_oldvalue_", i)] <- 'OldValue'
  names(temp_name)[names(temp_name) == paste0("hfc_logic_check_newvalue_", i)] <- 'NewValue'
  temp_name <- data.frame(temp_name$UUID, temp_name$Region, temp_name$District, temp_name$Settlement, temp_name$Enumerator, temp_name$Indicator, temp_name$Comment, temp_name$OldValue, temp_name$NewValue)
  names(temp_name)[names(temp_name) == 'temp_name.UUID'] <- 'UUID'
  names(temp_name)[names(temp_name) == 'temp_name.Region'] <- 'Region'
  names(temp_name)[names(temp_name) == 'temp_name.District'] <- 'District'
  names(temp_name)[names(temp_name) == 'temp_name.Settlement'] <- 'Settlement'
  names(temp_name)[names(temp_name) == 'temp_name.Enumerator'] <- 'Enumerator'
  names(temp_name)[names(temp_name) == paste0("temp_name.Indicator")] <- 'Indicator'
  names(temp_name)[names(temp_name) == paste0("temp_name.Comment")] <- 'Comment'
  names(temp_name)[names(temp_name) == paste0("temp_name.OldValue")] <- 'OldValue'
  names(temp_name)[names(temp_name) == paste0("temp_name.NewValue")] <- 'NewValue'
  dsa_hfc_issues_log <- rbind(dsa_hfc_issues_log, temp_name)
}

# 2.09    Check hard/soft constraints
# Contradiction - Population Estimates: Shelters, Households, Families
data$cccm_ratio_familiestoshelters<-data$cccm_populationestimates_families/data$cccm_populationestimates_shelters
data$cccm_ratio_individualstofamilies<-data$cccm_populationestimates_individuals/data$cccm_populationestimates_families

data$hfc_contradiction_check_indicator_cccm_populationestimates_familiestoshelters<-as.integer(0)
data$hfc_contradiction_check_indicator_cccm_populationestimates_familiestoshelters[data$cccm_ratio_familiestoshelters<1]<-as.integer(1)
data$hfc_contradiction_check_indicator_name_cccm_populationestimates_familiestoshelters<-as.character("Population Check: Families to Shelters")
data$hfc_contradiction_check_comment_cccm_populationestimates_familiestoshelters<-as.character(paste0("Number of families is ", data$cccm_populationestimates_families, " and number of shelters is ", data$cccm_populationestimates_shelters, ". Please confirm in Remote KII."))
data$hfc_contradiction_check_oldvalue_cccm_populationestimates_familiestoshelters<-as.character("Not Applicable")
data$hfc_contradiction_check_newvalue_cccm_populationestimates_familiestoshelters<-as.character("Not Applicable")

data$hfc_contradiction_check_indicator_cccm_populationestimates_individualstofamilies<-as.integer(0)
data$hfc_contradiction_check_indicator_cccm_populationestimates_individualstofamilies[data$cccm_ratio_individualstofamilies<1]<-as.integer(1)
data$hfc_contradiction_check_indicator_name_cccm_populationestimates_individualstofamilies<-as.character("Population Check: Individuals to Families")
data$hfc_contradiction_check_comment_cccm_populationestimates_individualstofamilies<-as.character(paste0("Number of individuals is ", data$cccm_populationestimates_individuals, " and number of families is ", data$cccm_populationestimates_families, ". Please confirm in Remote KII."))
data$hfc_contradiction_check_oldvalue_cccm_populationestimates_individualstofamilies<-as.character("Not Applicable")
data$hfc_contradiction_check_newvalue_cccm_populationestimates_individualstofamilies<-as.character("Not Applicable")

# Contradiction: Population Estimates: Number of Families Total against Number of Families Arrived minus Number of Families Departed
data$hfc_contradiction_check_indicator_cccm_populationestimates_families_arrived_departed<-as.integer(0)
data$hfc_contradiction_check_indicator_cccm_populationestimates_families_arrived_departed[data$cccm_populationestimates_families<(data$cccm_idps_arrived-data$cccm_idps_departed)]<-as.integer(1)
data$hfc_contradiction_check_indicator_name_cccm_populationestimates_families_arrived_departed<-as.character("Population Check: Total Families to Families Arrived and Families Departed")
data$hfc_contradiction_check_comment_cccm_populationestimates_families_arrived_departed<-as.character(paste0("Number of families total is ", data$cccm_populationestimates_families, " and number of families arrived is ", data$cccm_idps_arrived, " and number of families departed is ", data$cccm_idps_departed, ". Please confirm in Remote KII."))
data$hfc_contradiction_check_oldvalue_cccm_populationestimates_families_arrived_departed<-as.character("Not Applicable")
data$hfc_contradiction_check_newvalue_cccm_populationestimates_families_arrived_departed<-as.character("Not Applicable")

# Contradiction: Site Duration
data$cccm_idps_arrival_months[data$cccm_idps_arrival=="lessthanone"]<-as.integer(1)
data$cccm_idps_arrival_months[data$cccm_idps_arrival=="morethansix"]<-as.integer(6)
data$cccm_idps_arrival_months[data$cccm_idps_arrival=="onetothree"]<-as.integer(3)
data$cccm_idps_arrival_months[data$cccm_idps_arrival=="threetosix"]<-as.integer(6)
data$cccm_idps_departure_months[data$cccm_idps_departure=="lessthanone"]<-as.integer(1)
data$cccm_idps_departure_months[data$cccm_idps_departure=="morethansix"]<-as.integer(6)
data$cccm_idps_departure_months[data$cccm_idps_departure=="onetothree"]<-as.integer(3)
data$cccm_idps_departure_months[data$cccm_idps_departure=="threetosix"]<-as.integer(6)

data$hfc_contradiction_check_indicator_cccm_idps_siteduration_minus_arrival_departure_months<-as.integer(0)
data$hfc_contradiction_check_indicator_cccm_idps_siteduration_minus_arrival_departure_months[data$cccm_site_duration<data$cccm_idps_arrival_months]<-as.integer(1)
data$hfc_contradiction_check_indicator_cccm_idps_siteduration_minus_arrival_departure_months[data$cccm_site_duration<data$cccm_idps_departure_months]<-as.integer(1)

data$hfc_contradiction_check_indicator_name_cccm_idps_siteduration_minus_arrival_departure_months<-as.character("Site Duration Check")
data$hfc_contradiction_check_comment_cccm_idps_siteduration_minus_arrival_departure_months<-as.character(paste0("Number of months site has been established is ", data$cccm_site_duration, " and number of months that most IDPs have arrived is ", data$cccm_idps_arrival_months, " and number of months that most IDPs have departed is ", data$cccm_idps_departure_months, ". Please confirm in Remote KII."))
data$hfc_contradiction_check_oldvalue_cccm_idps_siteduration_minus_arrival_departure_months<-as.character("Not Applicable")
data$hfc_contradiction_check_newvalue_cccm_idps_siteduration_minus_arrival_departure_months<-as.character("Not Applicable")

# Women Present in Committees
data$cccm_womenpresentincommittees <- as.character("No")
data$cccm_womenscommitteepresent <- as.character("No")
data$cccm_womenpresentincommittees[data$cccm_committees_women=="yes"]<-as.character("Yes")
data$cccm_womenscommitteepresent[data$cccm_committees.cccm_committees_3==1]<-as.character("Yes")
data$hfc_contradiction_check_indicator_cccm_committees_women<-as.integer(0)
data$hfc_contradiction_check_indicator_cccm_committees_women[data$cccm_committees_women=="no" & data$cccm_committees.cccm_committees_3==1]<-as.integer(1)
data$hfc_contradiction_check_indicator_name_cccm_committees_women<-as.character("Camp Committees Check")
data$hfc_contradiction_check_comment_cccm_committees_women<-as.character(paste0("Women's Committee Present: ", data$cccm_womenscommitteepresent, " , Women Present in Committee: ", data$cccm_womenpresentincommittees, ". Please confirm in Remote KII."))
data$hfc_contradiction_check_oldvalue_cccm_committees_women<-as.character("Not Applicable")
data$hfc_contradiction_check_newvalue_cccm_committees_women<-as.character("Not Applicable")

data$hfc_contradiction_check_indicator_cccm_committees_women <- as.integer(0)
data$hfc_contradiction_check_indicator_cccm_committees_women[data$cccm_committees_women=="no" & data$cccm_committees.cccm_committees_3==1]<-as.integer(1)

# Distance to Services
data$distance_nfi <- data$nfi_access_distance_max - data$nfi_access_distance_min
data$distance_water <- data$water_access_distance_max - data$water_access_distance_min
data$distance_sanitation <- data$sanitation_access_distance_max - data$sanitation_access_distance_min
data$distance_hygiene <- data$hygiene_access_distance_max - data$hygiene_access_distance_min
data$distance_health <- data$health_access_distance_max - data$health_access_distance_min
data$distance_nutrition <- data$nutrition_access_distance_max - data$nutrition_access_distance_min
data$distance_education <- data$education_access_distance_max - data$education_access_distance_min
data$distance_foodsecurity <- data$foodsecurity_access_distance_max - data$foodsecurity_access_distance_min
data$distance <- rowMeans(data[,c("distance_nfi", "distance_water", "distance_sanitation", "distance_hygiene", "distance_health", "distance_nutrition", "distance_education", "distance_foodsecurity")], na.rm=TRUE)
data$settlement_size <- data$cccm_populationestimates_shelters / data$distance
data$hfc_contradiction_check_indicator_settlement_size<-as.integer(0)
data$hfc_contradiction_check_indicator_settlement_size[data$settlement_size>100] <- as.integer(1)
data$hfc_contradiction_check_indicator_name_settlement_size<-as.character("Contradiction Check: Settlement Size to Min/Max Minutes to Travel")
data$hfc_contradiction_check_comment_settlement_size<-as.character(paste0("Number of Shelters in relation to average min/max distance to travel doesn't make sense. Check with enumerator to make sure questions regarding mininum/maximum  minutes to travel are understood."))
data$hfc_contradiction_check_oldvalue_settlement_size<-as.character("Not Applicable")
data$hfc_contradiction_check_newvalue_settlement_size<-as.character("Not Applicable")

# Contradiction Check: Toilets
data$ratio_individualstotoilets <- data$cccm_populationestimates_individuals / data$sanitation_toilets_total
data$hfc_contradiction_check_indicator_numberoftoilets<-as.integer(0)
data$hfc_contradiction_check_indicator_numberoftoilets[data$numberoftoilets>40] <- as.integer(1)
data$hfc_contradiction_check_indicator_name_numberoftoilets<-as.character("Contradiction Check: Number of Toilets")
data$hfc_contradiction_check_comment_numberoftoilets<-as.character("Number of toilets in relation to number of individuals doesn't make sense. Check with enumerator to make sure questions regarding number of toilets are understood.")
data$hfc_contradiction_check_oldvalue_numberoftoilets<-as.character("Not Applicable")
data$hfc_contradiction_check_newvalue_numberoftoilets<-as.character("Not Applicable")

# Contradiction Check: Bathing Facilities
data$ratio_individualstobathingfacilities <- data$cccm_populationestimates_individuals / data$hygiene_bathingfacilities
data$hfc_contradiction_check_indicator_numberofbathingfacilities<-as.integer(0)
data$hfc_contradiction_check_indicator_numberofbathingfacilities[data$numberofbathingfacilities>40] <- as.integer(1)
data$hfc_contradiction_check_indicator_name_numberofbathingfacilities<-as.character("Contradiction Check: Number of Bathing Facilities")
data$hfc_contradiction_check_comment_numberofbathingfacilities<-as.character("Number of bathing facilities in relation to number of individuals doesn't make sense. Check with enumerator to make sure questions regarding number of bathing facilities are understood.")
data$hfc_contradiction_check_oldvalue_numberofbathingfacilities<-as.character("Not Applicable")
data$hfc_contradiction_check_newvalue_numberofbathingfacilities<-as.character("Not Applicable")

list_indicators_contradiction_check <- c("cccm_populationestimates_familiestoshelters",
                                         "cccm_populationestimates_individualstofamilies",
                                         "cccm_populationestimates_families_arrived_departed",
                                         "cccm_idps_siteduration_minus_arrival_departure_months",
                                         "cccm_committees_women",
                                         "settlement_size",
                                         "numberoftoilets",
                                         "numberofbathingfacilities")

for (i in list_indicators_contradiction_check){
  temp_name <- paste0("hfc_contradiction_check_", i)
  temp_name_indicator <- paste0("hfc_contradiction_check_indicator_", i)
  temp_name_indicator_name <- paste0("hfc_contradiction_check_indicator_name_", i)
  temp_name_comment <- paste0("hfc_contradiction_check_comment_", i)
  temp_name_oldvalue <- paste0("hfc_contradiction_check_oldvalue_", i)
  temp_name_newvalue <- paste0("hfc_contradiction_check_newvalue_", i)
  temp_name <- data.frame(data$X_uuid, data$localisation_region, data$localisation_district, data$localisation_settlement_name_local, data$deviceid, data[temp_name_indicator], data[temp_name_indicator_name], data[temp_name_comment], data[temp_name_oldvalue], data[temp_name_newvalue])
  temp_name <- subset(temp_name, data[temp_name_indicator]!=0)
  names(temp_name)[names(temp_name) == 'data.X_uuid'] <- 'UUID'
  names(temp_name)[names(temp_name) == 'data.localisation_region'] <- 'Region'
  names(temp_name)[names(temp_name) == 'data.localisation_district'] <- 'District'
  names(temp_name)[names(temp_name) == 'data.localisation_settlement_name_local'] <- 'Settlement'
  names(temp_name)[names(temp_name) == 'data.deviceid'] <- 'Enumerator'
  names(temp_name)[names(temp_name) == paste0("hfc_contradiction_check_indicator_name_", i)] <- 'Indicator'
  names(temp_name)[names(temp_name) == paste0("hfc_contradiction_check_comment_", i)] <- 'Comment'
  names(temp_name)[names(temp_name) == paste0("hfc_contradiction_check_oldvalue_", i)] <- 'OldValue'
  names(temp_name)[names(temp_name) == paste0("hfc_contradiction_check_newvalue_", i)] <- 'NewValue'
  temp_name <- data.frame(temp_name$UUID, temp_name$Region, temp_name$District, temp_name$Settlement, temp_name$Enumerator, temp_name$Indicator, temp_name$Comment, temp_name$OldValue, temp_name$NewValue)
  names(temp_name)[names(temp_name) == 'temp_name.UUID'] <- 'UUID'
  names(temp_name)[names(temp_name) == 'temp_name.Region'] <- 'Region'
  names(temp_name)[names(temp_name) == 'temp_name.District'] <- 'District'
  names(temp_name)[names(temp_name) == 'temp_name.Settlement'] <- 'Settlement'
  names(temp_name)[names(temp_name) == 'temp_name.Enumerator'] <- 'Enumerator'
  names(temp_name)[names(temp_name) == paste0("temp_name.Indicator")] <- 'Indicator'
  names(temp_name)[names(temp_name) == paste0("temp_name.Comment")] <- 'Comment'
  names(temp_name)[names(temp_name) == paste0("temp_name.OldValue")] <- 'OldValue'
  names(temp_name)[names(temp_name) == paste0("temp_name.NewValue")] <- 'NewValue'
  dsa_hfc_issues_log <- rbind(dsa_hfc_issues_log, temp_name)
}

# More Than Three: Impediments to Access
data$hfc_morethanthree_check_count_access_impediments_nfi <- data$nfi_access_impediments.impediments_populationgroups_1 + data$nfi_access_impediments.impediments_populationgroups_2 + data$nfi_access_impediments.impediments_populationgroups_3 + data$nfi_access_impediments.impediments_populationgroups_4 + data$nfi_access_impediments.impediments_populationgroups_5
data$hfc_morethanthree_check_indicator_access_impediments_nfi <- as.integer(0)
data$hfc_morethanthree_check_indicator_access_impediments_nfi[data$hfc_morethanthree_check_count_access_impediments_nfi>3] <- as.integer(1)
data$hfc_morethanthree_check_indicator_name_access_impediments_nfi<-as.character("More Than Three Check: Impediments to Accessing NFI Markets")
data$hfc_morethanthree_check_comment_access_impediments_nfi<-as.character(paste0("Number of selected groups with difficulty accessing NFI markets is ", data$hfc_morethanthree_check_count_access_impediments_nfi, " Please confirm in Remote KII."))
data$hfc_morethanthree_check_oldvalue_access_impediments_nfi<-as.character("Not Applicable")
data$hfc_morethanthree_check_newvalue_access_impediments_nfi<-as.character("Not Applicable")

data$hfc_morethanthree_check_count_access_impediments_water <- data$water_access_impediments.impediments_populationgroups_1 + data$water_access_impediments.impediments_populationgroups_2 + data$water_access_impediments.impediments_populationgroups_3 + data$water_access_impediments.impediments_populationgroups_4 + data$water_access_impediments.impediments_populationgroups_5
data$hfc_morethanthree_check_indicator_access_impediments_water <- as.integer(0)
data$hfc_morethanthree_check_indicator_access_impediments_water[data$hfc_morethanthree_check_count_access_impediments_water>3] <- as.integer(1)
data$hfc_morethanthree_check_indicator_name_access_impediments_water<-as.character("More Than Three Check: Impediments to Accessing Water Sources")
data$hfc_morethanthree_check_comment_access_impediments_water<-as.character(paste0("Number of selected groups with difficulty accessing water sources is ", data$hfc_morethanthree_check_count_access_impediments_water, " Please confirm in Remote KII."))
data$hfc_morethanthree_check_oldvalue_access_impediments_water<-as.character("Not Applicable")
data$hfc_morethanthree_check_newvalue_access_impediments_water<-as.character("Not Applicable")

data$hfc_morethanthree_check_count_access_impediments_sanitation <- data$sanitation_access_impediments.impediments_populationgroups_1 + data$sanitation_access_impediments.impediments_populationgroups_2 + data$sanitation_access_impediments.impediments_populationgroups_3 + data$sanitation_access_impediments.impediments_populationgroups_4 + data$sanitation_access_impediments.impediments_populationgroups_5
data$hfc_morethanthree_check_indicator_access_impediments_sanitation <- as.integer(0)
data$hfc_morethanthree_check_indicator_access_impediments_sanitation[data$hfc_morethanthree_check_count_access_impediments_sanitation>3] <- as.integer(1)
data$hfc_morethanthree_check_indicator_name_access_impediments_sanitation<-as.character("More Than Three Check: Impediments to Accessing Latrines")
data$hfc_morethanthree_check_comment_access_impediments_sanitation<-as.character(paste0("Number of selected groups with difficulty accessing latrines is ", data$hfc_morethanthree_check_count_access_impediments_sanitation, " Please confirm in Remote KII."))
data$hfc_morethanthree_check_oldvalue_access_impediments_sanitation<-as.character("Not Applicable")
data$hfc_morethanthree_check_newvalue_access_impediments_sanitation<-as.character("Not Applicable")

data$hfc_morethanthree_check_count_access_impediments_hygiene <- data$hygiene_access_impediments.impediments_populationgroups_1 + data$hygiene_access_impediments.impediments_populationgroups_2 + data$hygiene_access_impediments.impediments_populationgroups_3 + data$hygiene_access_impediments.impediments_populationgroups_4 + data$hygiene_access_impediments.impediments_populationgroups_5
data$hfc_morethanthree_check_indicator_access_impediments_hygiene <- as.integer(0)
data$hfc_morethanthree_check_indicator_access_impediments_hygiene[data$hfc_morethanthree_check_count_access_impediments_hygiene>3] <- as.integer(1)
data$hfc_morethanthree_check_indicator_name_access_impediments_hygiene<-as.character("More Than Three Check: Impediments to Accessing Bathing Facilities")
data$hfc_morethanthree_check_comment_access_impediments_hygiene<-as.character(paste0("Number of selected groups with difficulty accessing bathing facilities is ", data$hfc_morethanthree_check_count_access_impediments_hygiene, " Please confirm in Remote KII."))
data$hfc_morethanthree_check_oldvalue_access_impediments_hygiene<-as.character("Not Applicable")
data$hfc_morethanthree_check_newvalue_access_impediments_hygiene<-as.character("Not Applicable")

data$hfc_morethanthree_check_count_access_impediments_health <- data$health_access_impediments.impediments_populationgroups_1 + data$health_access_impediments.impediments_populationgroups_2 + data$health_access_impediments.impediments_populationgroups_3 + data$health_access_impediments.impediments_populationgroups_4 + data$health_access_impediments.impediments_populationgroups_5
data$hfc_morethanthree_check_indicator_access_impediments_health <- as.integer(0)
data$hfc_morethanthree_check_indicator_access_impediments_health[data$hfc_morethanthree_check_count_access_impediments_health>3] <- as.integer(1)
data$hfc_morethanthree_check_indicator_name_access_impediments_health<-as.character("More Than Three Check: Impediments to Accessing Bathing Facilities")
data$hfc_morethanthree_check_comment_access_impediments_health<-as.character(paste0("Number of selected groups with difficulty accessing bathing facilities is ", data$hfc_morethanthree_check_count_access_impediments_health, " Please confirm in Remote KII."))
data$hfc_morethanthree_check_oldvalue_access_impediments_health<-as.character("Not Applicable")
data$hfc_morethanthree_check_newvalue_access_impediments_health<-as.character("Not Applicable")

data$hfc_morethanthree_check_count_access_impediments_nutrition <- data$nutrition_access_impediments.impediments_populationgroups_1 + data$nutrition_access_impediments.impediments_populationgroups_2 + data$nutrition_access_impediments.impediments_populationgroups_3 + data$nutrition_access_impediments.impediments_populationgroups_4 + data$nutrition_access_impediments.impediments_populationgroups_5
data$hfc_morethanthree_check_indicator_access_impediments_nutrition <- as.integer(0)
data$hfc_morethanthree_check_indicator_access_impediments_nutrition[data$hfc_morethanthree_check_count_access_impediments_nutrition>3] <- as.integer(1)
data$hfc_morethanthree_check_indicator_name_access_impediments_nutrition<-as.character("More Than Three Check: Impediments to Accessing Nutrition Services")
data$hfc_morethanthree_check_comment_access_impediments_nutrition<-as.character(paste0("Number of selected groups with difficulty accessing nutrition services is ", data$hfc_morethanthree_check_count_access_impediments_nutrition, " Please confirm in Remote KII."))
data$hfc_morethanthree_check_oldvalue_access_impediments_nutrition<-as.character("Not Applicable")
data$hfc_morethanthree_check_newvalue_access_impediments_nutrition<-as.character("Not Applicable")

data$hfc_morethanthree_check_count_access_impediments_education <- data$education_access_impediments.impediments_populationgroups_1 + data$education_access_impediments.impediments_populationgroups_2 + data$education_access_impediments.impediments_populationgroups_3 + data$education_access_impediments.impediments_populationgroups_4 + data$education_access_impediments.impediments_populationgroups_5
data$hfc_morethanthree_check_indicator_access_impediments_education <- as.integer(0)
data$hfc_morethanthree_check_indicator_access_impediments_education[data$hfc_morethanthree_check_count_access_impediments_education>3] <- as.integer(1)
data$hfc_morethanthree_check_indicator_name_access_impediments_education<-as.character("More Than Three Check: Impediments to Accessing Education Facilities")
data$hfc_morethanthree_check_comment_access_impediments_education<-as.character(paste0("Number of selected groups with difficulty accessing education services is ", data$hfc_morethanthree_check_count_access_impediments_education, " Please confirm in Remote KII."))
data$hfc_morethanthree_check_oldvalue_access_impediments_education<-as.character("Not Applicable")
data$hfc_morethanthree_check_newvalue_access_impediments_education<-as.character("Not Applicable")

data$hfc_morethanthree_check_count_access_impediments_foodsecurity <- data$foodsecurity_access_impediments.impediments_populationgroups_1 + data$foodsecurity_access_impediments.impediments_populationgroups_2 + data$foodsecurity_access_impediments.impediments_populationgroups_3 + data$foodsecurity_access_impediments.impediments_populationgroups_4 + data$foodsecurity_access_impediments.impediments_populationgroups_5
data$hfc_morethanthree_check_indicator_access_impediments_foodsecurity <- as.integer(0)
data$hfc_morethanthree_check_indicator_access_impediments_foodsecurity[data$hfc_morethanthree_check_count_access_impediments_foodsecurity>3] <- as.integer(1)
data$hfc_morethanthree_check_indicator_name_access_impediments_foodsecurity<-as.character("More Than Three Check: Impediments to Accessing Food Markets")
data$hfc_morethanthree_check_comment_access_impediments_foodsecurity<-as.character(paste0("Number of selected groups with difficulty accessing food markets is ", data$hfc_morethanthree_check_count_access_impediments_foodsecurity, " Please confirm in Remote KII."))
data$hfc_morethanthree_check_oldvalue_access_impediments_foodsecurity<-as.character("Not Applicable")
data$hfc_morethanthree_check_newvalue_access_impediments_foodsecurity<-as.character("Not Applicable")

data$hfc_morethanthree_check_count_access_impediments_support <- data$support_access_impediments.impediments_populationgroups_1 + data$support_access_impediments.impediments_populationgroups_2 + data$support_access_impediments.impediments_populationgroups_3 + data$support_access_impediments.impediments_populationgroups_4 + data$support_access_impediments.impediments_populationgroups_5
data$hfc_morethanthree_check_indicator_access_impediments_support <- as.integer(0)
data$hfc_morethanthree_check_indicator_access_impediments_support[data$hfc_morethanthree_check_count_access_impediments_support>3] <- as.integer(1)
data$hfc_morethanthree_check_indicator_name_access_impediments_support<-as.character("More Than Three Check: Impediments to Accessing Support Activities")
data$hfc_morethanthree_check_comment_access_impediments_support<-as.character(paste0("Number of selected groups with difficulty accessing support activities is ", data$hfc_morethanthree_check_count_access_impediments_support, " Please confirm in Remote KII."))
data$hfc_morethanthree_check_oldvalue_access_impediments_support<-as.character("Not Applicable")
data$hfc_morethanthree_check_newvalue_access_impediments_support<-as.character("Not Applicable")

data$hfc_morethanthree_check_count_access_impediments_aap <- data$aap_access_impediments.impediments_populationgroups_1 + data$aap_access_impediments.impediments_populationgroups_2 + data$aap_access_impediments.impediments_populationgroups_3 + data$aap_access_impediments.impediments_populationgroups_4 + data$aap_access_impediments.impediments_populationgroups_5
data$hfc_morethanthree_check_indicator_access_impediments_aap <- as.integer(0)
data$hfc_morethanthree_check_indicator_access_impediments_aap[data$hfc_morethanthree_check_count_access_impediments_aap>3] <- as.integer(1)
data$hfc_morethanthree_check_indicator_name_access_impediments_aap<-as.character("More Than Three Check: Impediments to Accessing Feedback Mechanisms")
data$hfc_morethanthree_check_comment_access_impediments_aap<-as.character(paste0("Number of selected groups with difficulty accessing feedback mechanisms is ", data$hfc_morethanthree_check_count_access_impediments_aap, " Please confirm in Remote KII."))
data$hfc_morethanthree_check_oldvalue_access_impediments_aap<-as.character("Not Applicable")
data$hfc_morethanthree_check_newvalue_access_impediments_aap<-as.character("Not Applicable")

# More Than Three: Select Multiple
data$hfc_morethanthree_check_count_shelter_types <- data$shelter_types.shelter_types_1 + data$shelter_types.shelter_types_2 + data$shelter_types.shelter_types_3 + data$shelter_types.shelter_types_4 + data$shelter_types.shelter_types_5 + data$shelter_types.shelter_types_6 + data$shelter_types.shelter_types_7 + data$shelter_types.shelter_types_8
data$hfc_morethanthree_check_indicator_shelter_types <- as.integer(0)
data$hfc_morethanthree_check_indicator_shelter_types[data$hfc_morethanthree_check_count_shelter_types>3] <- as.integer(1)
data$hfc_morethanthree_check_indicator_name_shelter_types<-as.character("More Than Three Check: Shelter Types")
data$hfc_morethanthree_check_comment_shelter_types<-as.character(paste0("Number of shelter types present is ", data$hfc_morethanthree_check_count_shelter_types, " Please confirm in Remote KII."))
data$hfc_morethanthree_check_oldvalue_shelter_types<-as.character("Not Applicable")
data$hfc_morethanthree_check_newvalue_shelter_types<-as.character("Not Applicable")

data$hfc_morethanthree_check_count_health_facilities <- data$health_facilities.health_facilities_1 + data$health_facilities.health_facilities_2 + data$health_facilities.health_facilities_3 + data$health_facilities.health_facilities_4 + data$health_facilities.health_facilities_5 + data$health_facilities.health_facilities_6 + data$health_facilities.health_facilities_7
data$hfc_morethanthree_check_indicator_health_facilities <- as.integer(0)
data$hfc_morethanthree_check_indicator_health_facilities[data$hfc_morethanthree_check_count_health_facilities>3] <- as.integer(1)
data$hfc_morethanthree_check_indicator_name_health_facilities<-as.character("More Than Three Check: Health Facilities")
data$hfc_morethanthree_check_comment_health_facilities<-as.character(paste0("Number of health facilities available is ", data$hfc_morethanthree_check_count_health_facilities, " Please confirm in Remote KII."))
data$hfc_morethanthree_check_oldvalue_health_facilities<-as.character("Not Applicable")
data$hfc_morethanthree_check_newvalue_health_facilities<-as.character("Not Applicable")

data$hfc_morethanthree_check_count_health_services <- data$health_services.health_services_1 + data$health_services.health_services_2 + data$health_services.health_services_3 + data$health_services.health_services_4 + data$health_services.health_services_5 + data$health_services.health_services_6 + data$health_services.health_services_7
data$hfc_morethanthree_check_indicator_health_services <- as.integer(0)
data$hfc_morethanthree_check_indicator_health_services[data$hfc_morethanthree_check_count_health_services>3] <- as.integer(1)
data$hfc_morethanthree_check_indicator_name_health_services<-as.character("More Than Three Check: Health Services")
data$hfc_morethanthree_check_comment_health_services<-as.character(paste0("Number of health services available is ", data$hfc_morethanthree_check_count_health_services, " Please confirm in Remote KII."))
data$hfc_morethanthree_check_oldvalue_health_services<-as.character("Not Applicable")
data$hfc_morethanthree_check_newvalue_health_services<-as.character("Not Applicable")

data$hfc_morethanthree_check_count_health_problems <- data$health_problems.health_problems_1 + data$health_problems.health_problems_2 + data$health_problems.health_problems_3 + data$health_problems.health_problems_4 + data$health_problems.health_problems_5 + data$health_problems.health_problems_6 + data$health_problems.health_problems_7 + data$health_problems.health_problems_8
data$hfc_morethanthree_check_indicator_health_problems <- as.integer(0)
data$hfc_morethanthree_check_indicator_health_problems[data$hfc_morethanthree_check_count_health_problems>3] <- as.integer(1)
data$hfc_morethanthree_check_indicator_name_health_problems<-as.character("More Than Three Check: Health Problems")
data$hfc_morethanthree_check_comment_health_problems<-as.character(paste0("Number of health problems common is ", data$hfc_morethanthree_check_count_health_problems, " Please confirm in Remote KII."))
data$hfc_morethanthree_check_oldvalue_health_problems<-as.character("Not Applicable")
data$hfc_morethanthree_check_newvalue_health_problems<-as.character("Not Applicable")

data$hfc_morethanthree_check_count_health_problems <- 
data$hfc_morethanthree_check_indicator_health_problems <- as.integer(0)
data$hfc_morethanthree_check_indicator_health_problems[data$hfc_morethanthree_check_count_health_problems>3] <- as.integer(1)
data$hfc_morethanthree_check_indicator_name_health_problems<-as.character("More Than Three Check: Health Problems")
data$hfc_morethanthree_check_comment_health_problems<-as.character(paste0("Number of nutrition services available is ", data$hfc_morethanthree_check_count_health_problems, " Please confirm in Remote KII."))
data$hfc_morethanthree_check_oldvalue_health_problems<-as.character("Not Applicable")
data$hfc_morethanthree_check_newvalue_health_problems<-as.character("Not Applicable")
  
data$hfc_morethanthree_check_count_nutrition_services <- data$nutrition_services.nutrition_services_1 + data$nutrition_services.nutrition_services_2 + data$nutrition_services.nutrition_services_3 + data$nutrition_services.nutrition_services_4 + data$nutrition_services.nutrition_services_5 + data$nutrition_services.nutrition_services_6
data$hfc_morethanthree_check_indicator_nutrition_services <- as.integer(0)
data$hfc_morethanthree_check_indicator_nutrition_services[data$hfc_morethanthree_check_count_nutrition_services>3] <- as.integer(1)
data$hfc_morethanthree_check_indicator_name_nutrition_services<-as.character("More Than Three Check: Nutrition Services")
data$hfc_morethanthree_check_comment_nutrition_services<-as.character(paste0("Number of nutrition services available is ", data$hfc_morethanthree_check_count_nutrition_services, " Please confirm in Remote KII."))
data$hfc_morethanthree_check_oldvalue_nutrition_services<-as.character("Not Applicable")
data$hfc_morethanthree_check_newvalue_nutrition_services<-as.character("Not Applicable")

data$hfc_morethanthree_check_count_education_facilities <- data$education_facilities.education_facilities_1 + data$education_facilities.education_facilities_2 + data$education_facilities.education_facilities_3 + data$education_facilities.education_facilities_4
data$hfc_morethanthree_check_indicator_education_facilities <- as.integer(0)
data$hfc_morethanthree_check_indicator_education_facilities[data$hfc_morethanthree_check_count_education_facilities>3] <- as.integer(1)
data$hfc_morethanthree_check_indicator_name_education_facilities<-as.character("More Than Three Check: Education Facilities")
data$hfc_morethanthree_check_comment_education_facilities<-as.character(paste0("Number of education facilities available is ", data$hfc_morethanthree_check_count_education_facilities, " Please confirm in Remote KII."))
data$hfc_morethanthree_check_oldvalue_education_facilities<-as.character("Not Applicable")
data$hfc_morethanthree_check_newvalue_education_facilities<-as.character("Not Applicable")

data$hfc_morethanthree_check_count_protection_incidents <- data$protection_incidents.protection_incidents_1 + data$protection_incidents.protection_incidents_2 + data$protection_incidents.protection_incidents_3 + data$protection_incidents.protection_incidents_4 + data$protection_incidents.protection_incidents_5 + data$protection_incidents.protection_incidents_6 + data$protection_incidents.protection_incidents_7 + data$protection_incidents.protection_incidents_8 + data$protection_incidents.protection_incidents_9 + data$protection_incidents.protection_incidents_10 + data$protection_incidents.protection_incidents_11 + data$protection_incidents.protection_incidents_12 + data$protection_incidents.protection_incidents_13 + data$protection_incidents.protection_incidents_14 + data$protection_incidents.protection_incidents_15
data$hfc_morethanthree_check_indicator_protection_incidents <- as.integer(0)
data$hfc_morethanthree_check_indicator_protection_incidents[data$hfc_morethanthree_check_count_protection_incidents>3] <- as.integer(1)
data$hfc_morethanthree_check_indicator_name_protection_incidents<-as.character("More Than Three Check: Protection Incidents")
data$hfc_morethanthree_check_comment_protection_incidents<-as.character(paste0("Number of protection incidents that have occurred is ", data$hfc_morethanthree_check_count_protection_incidents, " Please confirm in Remote KII."))
data$hfc_morethanthree_check_oldvalue_protection_incidents<-as.character("Not Applicable")
data$hfc_morethanthree_check_newvalue_protection_incidents<-as.character("Not Applicable")

data$hfc_morethanthree_check_count_support <- data$support.support_1 + data$support.support_2 + data$support.support_3 + data$support.support_4 + data$support.support_5 + data$support.support_6 + data$support.support_7 + data$support.support_8 + data$support.support_9 + data$support.support_10 + data$support.support_11 + data$support.support_12 + data$support.support_13 + data$support.support_14
data$hfc_morethanthree_check_indicator_support <- as.integer(0)
data$hfc_morethanthree_check_indicator_support[data$hfc_morethanthree_check_count_support>3] <- as.integer(1)
data$hfc_morethanthree_check_indicator_name_support<-as.character("More Than Three Check: Support Activities")
data$hfc_morethanthree_check_comment_support<-as.character(paste0("Number of nutrition support activities is ", data$hfc_morethanthree_check_count_support, " Please confirm in Remote KII."))
data$hfc_morethanthree_check_oldvalue_support<-as.character("Not Applicable")
data$hfc_morethanthree_check_newvalue_support<-as.character("Not Applicable")

data$hfc_morethanthree_check_count_aap_informationsources <- data$aap_informationsources.aap_informationsources_1 + data$aap_informationsources.aap_informationsources_2 + data$aap_informationsources.aap_informationsources_3 + data$aap_informationsources.aap_informationsources_4 + data$aap_informationsources.aap_informationsources_5 + data$aap_informationsources.aap_informationsources_6 + data$aap_informationsources.aap_informationsources_7 + data$aap_informationsources.aap_informationsources_8
data$hfc_morethanthree_check_indicator_aap_informationsources <- as.integer(0)
data$hfc_morethanthree_check_indicator_aap_informationsources[data$hfc_morethanthree_check_count_aap_informationsources>3] <- as.integer(1)
data$hfc_morethanthree_check_indicator_name_aap_informationsources<-as.character("More Than Three Check: Information Sources")
data$hfc_morethanthree_check_comment_aap_informationsources<-as.character(paste0("Number of information sources available is ", data$hfc_morethanthree_check_count_aap_informationsources, " Please confirm in Remote KII."))
data$hfc_morethanthree_check_oldvalue_aap_informationsources<-as.character("Not Applicable")
data$hfc_morethanthree_check_newvalue_aap_informationsources<-as.character("Not Applicable")

data$hfc_morethanthree_check_count_aap_informationsources_pwd <- data$aap_informationsources_pwd.aap_informationsources_pwd_1 + data$aap_informationsources_pwd.aap_informationsources_pwd_2 + data$aap_informationsources_pwd.aap_informationsources_pwd_3 + data$aap_informationsources_pwd.aap_informationsources_pwd_4 + data$aap_informationsources_pwd.aap_informationsources_pwd_5
data$hfc_morethanthree_check_indicator_aap_informationsources_pwd <- as.integer(0)
data$hfc_morethanthree_check_indicator_aap_informationsources_pwd[data$hfc_morethanthree_check_count_aap_informationsources_pwd>3] <- as.integer(1)
data$hfc_morethanthree_check_indicator_name_aap_informationsources_pwd<-as.character("More Than Three Check: Information Sources for Persons with Difficulties")
data$hfc_morethanthree_check_comment_aap_informationsources_pwd<-as.character(paste0("Number of information sources for persons with difficulties available is ", data$hfc_morethanthree_check_count_aap_informationsources_pwd, " Please confirm in Remote KII."))
data$hfc_morethanthree_check_oldvalue_aap_informationsources_pwd<-as.character("Not Applicable")
data$hfc_morethanthree_check_newvalue_aap_informationsources_pwd<-as.character("Not Applicable")

list_indicators_morethanthree_check <- c("access_impediments_nfi",
                                         "access_impediments_water",
                                         "access_impediments_sanitation",
                                         "access_impediments_hygiene",
                                         "access_impediments_health",
                                         "access_impediments_nutrition",
                                         "access_impediments_education",
                                         "access_impediments_foodsecurity",
                                         "access_impediments_support",
                                         "access_impediments_aap",
                                         "shelter_types",
                                         "health_facilities",
                                         "health_services",
                                         "health_problems",
                                         "protection_incidents",
                                         "support",
                                         "aap_informationsources",
                                         "aap_informationsources_pwd"
)

for (i in list_indicators_morethanthree_check){
  temp_name <- paste0("hfc_morethanthree_check_", i)
  temp_name_indicator <- paste0("hfc_morethanthree_check_indicator_", i)
  temp_name_indicator_name <- paste0("hfc_morethanthree_check_indicator_name_", i)
  temp_name_comment <- paste0("hfc_morethanthree_check_comment_", i)
  temp_name_oldvalue <- paste0("hfc_morethanthree_check_oldvalue_", i)
  temp_name_newvalue <- paste0("hfc_morethanthree_check_newvalue_", i)
  temp_name <- data.frame(data$X_uuid, data$localisation_region, data$localisation_district, data$localisation_settlement_name_local, data$deviceid, data[temp_name_indicator], data[temp_name_indicator_name], data[temp_name_comment], data[temp_name_oldvalue], data[temp_name_newvalue])
  temp_name <- subset(temp_name, data[temp_name_indicator]!=0)
  names(temp_name)[names(temp_name) == 'data.X_uuid'] <- 'UUID'
  names(temp_name)[names(temp_name) == 'data.localisation_region'] <- 'Region'
  names(temp_name)[names(temp_name) == 'data.localisation_district'] <- 'District'
  names(temp_name)[names(temp_name) == 'data.localisation_settlement_name_local'] <- 'Settlement'
  names(temp_name)[names(temp_name) == 'data.deviceid'] <- 'Enumerator'
  names(temp_name)[names(temp_name) == paste0("hfc_morethanthree_check_indicator_name_", i)] <- 'Indicator'
  names(temp_name)[names(temp_name) == paste0("hfc_morethanthree_check_comment_", i)] <- 'Comment'
  names(temp_name)[names(temp_name) == paste0("hfc_morethanthree_check_oldvalue_", i)] <- 'OldValue'
  names(temp_name)[names(temp_name) == paste0("hfc_morethanthree_check_newvalue_", i)] <- 'NewValue'
  temp_name <- data.frame(temp_name$UUID, temp_name$Region, temp_name$District, temp_name$Settlement, temp_name$Enumerator, temp_name$Indicator, temp_name$Comment, temp_name$OldValue, temp_name$NewValue)
  names(temp_name)[names(temp_name) == 'temp_name.UUID'] <- 'UUID'
  names(temp_name)[names(temp_name) == 'temp_name.Region'] <- 'Region'
  names(temp_name)[names(temp_name) == 'temp_name.District'] <- 'District'
  names(temp_name)[names(temp_name) == 'temp_name.Settlement'] <- 'Settlement'
  names(temp_name)[names(temp_name) == 'temp_name.Enumerator'] <- 'Enumerator'
  names(temp_name)[names(temp_name) == paste0("temp_name.Indicator")] <- 'Indicator'
  names(temp_name)[names(temp_name) == paste0("temp_name.Comment")] <- 'Comment'
  names(temp_name)[names(temp_name) == paste0("temp_name.OldValue")] <- 'OldValue'
  names(temp_name)[names(temp_name) == paste0("temp_name.NewValue")] <- 'NewValue'
  dsa_hfc_issues_log <- rbind(dsa_hfc_issues_log, temp_name)
}

# Observation Checks
# Observation: Public Lighting
data$hfc_observation_check_indicator_publiclighting <- as.integer(0)
data$hfc_observation_check_indicator_publiclighting[data$shelter_publiclighting=="no" & data$observation_publiclighting=="yes"] <- as.integer(1)
data$hfc_observation_check_indicator_name_publiclighting<-as.character("Observation Check: Public Lighting")
data$hfc_observation_check_comment_publiclighting<-as.character(paste0("Key Informant reported public lighting: ", data$shelter_publiclighting, "Enumerator observed public lighting: ", data$observation_publiclighting," Please confirm in Remote KII."))
data$hfc_observation_check_oldvalue_publiclighting<-as.character("Not Applicable")
data$hfc_observation_check_newvalue_publiclighting<-as.character("Not Applicable")

# Observation: Shelter Damage
data$hfc_observation_check_indicator_shelterdamage <- as.integer(0)
data$hfc_observation_check_indicator_shelterdamage[(data$shelter_fire_destroyed==0 | data$shelter_fire_destroyed==NA | data$shelter_flood_destroyed==0 | data$shelter_flood_destroyed==NA) & data$observation_shelters_fire=="yes"] <- as.integer(1)
data$hfc_observation_check_indicator_name_shelterdamage<-as.character("Observation Check: Shelter Damage")
data$hfc_observation_check_comment_shelterdamage<-as.character(paste0("Key Informant reported shelter damage: ", data$shelter_shelterdamage, "Enumerator observed shelter damage ", data$observation_shelterdamage," Please confirm in Remote KII."))
data$hfc_observation_check_oldvalue_shelterdamage<-as.character("Not Applicable")
data$hfc_observation_check_newvalue_shelterdamage<-as.character("Not Applicable")

# Observation: Faecal Matter
data$hfc_observation_check_indicator_faecalmatter <- as.integer(0)
data$hfc_observation_check_indicator_faecalmatter[data$sanitation_faecalmatter=="no" & data$observation_faecalmatter=="yes"] <- as.integer(1)
data$hfc_observation_check_indicator_name_faecalmatter<-as.character("Observation Check: Faecal Matter")
data$hfc_observation_check_comment_faecalmatter<-as.character(paste0("Key Informant reported faecal matter: ", data$shelter_faecalmatter, "Enumerator observed faecal matter ", data$observation_faecalmatter," Please confirm in Remote KII."))
data$hfc_observation_check_oldvalue_faecalmatter<-as.character("Not Applicable")
data$hfc_observation_check_newvalue_faecalmatter<-as.character("Not Applicable")

# Observation: Burning Waste
data$hfc_observation_check_indicator_burningwaste <- as.integer(0)
data$hfc_observation_check_indicator_burningwaste[data$sanitation_burningwaste=="no" & data$observation_burningwaste=="yes"] <- as.integer(1)
data$hfc_observation_check_indicator_name_burningwaste<-as.character("Observation Check: Burning Waste")
data$hfc_observation_check_comment_burningwaste<-as.character(paste0("Key Informant reported burning waste: ", data$shelter_burningwaste, "Enumerator observed burning waste ", data$observation_burningwaste," Please confirm in Remote KII."))
data$hfc_observation_check_oldvalue_burningwaste<-as.character("Not Applicable")
data$hfc_observation_check_newvalue_burningwaste<-as.character("Not Applicable")

list_indicators_observation_check <- c("publiclighting",
                                       "shelterdamage",
                                       "faecalmatter",
                                       "burningwaste"
)

for (i in list_indicators_observation_check){
  temp_name <- paste0("hfc_observation_check_", i)
  temp_name_indicator <- paste0("hfc_observation_check_indicator_", i)
  temp_name_indicator_name <- paste0("hfc_observation_check_indicator_name_", i)
  temp_name_comment <- paste0("hfc_observation_check_comment_", i)
  temp_name_oldvalue <- paste0("hfc_observation_check_oldvalue_", i)
  temp_name_newvalue <- paste0("hfc_observation_check_newvalue_", i)
  temp_name <- data.frame(data$X_uuid, data$localisation_region, data$localisation_district, data$localisation_settlement_name_local, data$deviceid, data[temp_name_indicator], data[temp_name_indicator_name], data[temp_name_comment], data[temp_name_oldvalue], data[temp_name_newvalue])
  temp_name <- subset(temp_name, data[temp_name_indicator]!=0)
  names(temp_name)[names(temp_name) == 'data.X_uuid'] <- 'UUID'
  names(temp_name)[names(temp_name) == 'data.localisation_region'] <- 'Region'
  names(temp_name)[names(temp_name) == 'data.localisation_district'] <- 'District'
  names(temp_name)[names(temp_name) == 'data.localisation_settlement_name_local'] <- 'Settlement'
  names(temp_name)[names(temp_name) == 'data.deviceid'] <- 'Enumerator'
  names(temp_name)[names(temp_name) == paste0("hfc_observation_check_indicator_name_", i)] <- 'Indicator'
  names(temp_name)[names(temp_name) == paste0("hfc_observation_check_comment_", i)] <- 'Comment'
  names(temp_name)[names(temp_name) == paste0("hfc_observation_check_oldvalue_", i)] <- 'OldValue'
  names(temp_name)[names(temp_name) == paste0("hfc_observation_check_newvalue_", i)] <- 'NewValue'
  temp_name <- data.frame(temp_name$UUID, temp_name$Region, temp_name$District, temp_name$Settlement, temp_name$Enumerator, temp_name$Indicator, temp_name$Comment, temp_name$OldValue, temp_name$NewValue)
  names(temp_name)[names(temp_name) == 'temp_name.UUID'] <- 'UUID'
  names(temp_name)[names(temp_name) == 'temp_name.Region'] <- 'Region'
  names(temp_name)[names(temp_name) == 'temp_name.District'] <- 'District'
  names(temp_name)[names(temp_name) == 'temp_name.Settlement'] <- 'Settlement'
  names(temp_name)[names(temp_name) == 'temp_name.Enumerator'] <- 'Enumerator'
  names(temp_name)[names(temp_name) == paste0("temp_name.Indicator")] <- 'Indicator'
  names(temp_name)[names(temp_name) == paste0("temp_name.Comment")] <- 'Comment'
  names(temp_name)[names(temp_name) == paste0("temp_name.OldValue")] <- 'OldValue'
  names(temp_name)[names(temp_name) == paste0("temp_name.NewValue")] <- 'NewValue'
  dsa_hfc_issues_log <- rbind(dsa_hfc_issues_log, temp_name)
}

# 2.10    Check specify other variables for items that were mismarked as 'other'
# Other - CCCM: Management (Select Multiple)
data$hfc_other_check_indicator_cccm_management <- as.integer(0)
data$hfc_other_check_indicator_cccm_management[data$cccm_management.other==1] <- as.integer(1)
data$hfc_other_check_indicator_name_cccm_management<-as.character("Other Check: CCCM - Management")
data$hfc_other_check_comment_cccm_management<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_cccm_management<-data$cccm_management_other
data$hfc_other_check_newvalue_cccm_management<-as.character("")

# Other - CCCM: Committees (Select Multiple)
data$hfc_other_check_indicator_cccm_committees <- as.integer(0)
data$hfc_other_check_indicator_cccm_committees[data$cccm_committees.other==1] <- as.integer(1)
data$hfc_other_check_indicator_name_cccm_committees<-as.character("Other Check: CCCM - Committees")
data$hfc_other_check_comment_cccm_committees<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_cccm_committees<-data$cccm_committees_other
data$hfc_other_check_newvalue_cccm_committees<-as.character("")

# Other - Evictions: Landowner (Select One)
data$hfc_other_check_indicator_evictions_landowner <- as.integer(0)
data$hfc_other_check_indicator_evictions_landowner[data$evictions_landowner=="other"] <- as.integer(1)
data$hfc_other_check_indicator_name_evictions_landowner<-as.character("Other Check: Evictions - Landowner")
data$hfc_other_check_comment_evictions_landowner<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_evictions_landowner<-data$evictions_landowner_other
data$hfc_other_check_newvalue_evictions_landowner<-as.character("")

# Other - Evictions: Tenure Agreement Holder (Select Multiple)
data$hfc_other_check_indicator_evictions_tenureagreement_holder <- as.integer(0)
data$hfc_other_check_indicator_evictions_tenureagreement_holder[data$evictions_tenureagreement_holder.other==1] <- as.integer(1)
data$hfc_other_check_indicator_name_evictions_tenureagreement_holder<-as.character("Other Check: Evictions - Tenure Agreement Holder")
data$hfc_other_check_comment_evictions_tenureagreement_holder<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_evictions_tenureagreement_holder<-data$evictions_tenureagreement_holder_other
data$hfc_other_check_newvalue_evictions_tenureagreement_holder<-as.character("")

# Other - Evictions: Tenure Agreement Rentpayment (Select One)
data$hfc_other_check_indicator_evictions_tenureagreement_rentpayment <- as.integer(0)
data$hfc_other_check_indicator_evictions_tenureagreement_rentpayment[data$evictions_tenureagreement_rentpayment=="other"] <- as.integer(1)
data$hfc_other_check_indicator_name_evictions_tenureagreement_rentpayment<-as.character("Other Check: Evictions - Rent Payment")
data$hfc_other_check_comment_evictions_tenureagreement_rentpayment<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_evictions_tenureagreement_rentpayment<-data$evictions_tenureagreement_rentpayment_other
data$hfc_other_check_newvalue_evictions_tenureagreement_rentpayment<-as.character("")

# Other - Water: Primary Source (Select One)
data$hfc_other_check_indicator_water_sources_primary <- as.integer(0)
data$hfc_other_check_indicator_water_sources_primary[data$water_sources_primary=="other"] <- as.integer(1)
data$hfc_other_check_indicator_name_water_sources_primary<-as.character("Other Check: Water - Primary Source")
data$hfc_other_check_comment_water_sources_primary<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_water_sources_primary<-data$water_sources_primary_other
data$hfc_other_check_newvalue_water_sources_primary<-as.character("")

# Other - Water: Secondary Source (Select One)
data$hfc_other_check_indicator_water_sources_secondary <- as.integer(0)
data$hfc_other_check_indicator_water_sources_secondary[data$water_sources_secondary=="other"] <- as.integer(1)
data$hfc_other_check_indicator_name_water_sources_secondary<-as.character("Other Check: Water - Secondary Source")
data$hfc_other_check_comment_water_sources_secondary<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_water_sources_secondary<-data$water_sources_secondary_other
data$hfc_other_check_newvalue_water_sources_secondary<-as.character("")

# Other - Water: Domestic Source (Select One)
data$hfc_other_check_indicator_water_sources_domestic <- as.integer(0)
data$hfc_other_check_indicator_water_sources_domestic[data$water_sources_domestic=="other"] <- as.integer(1)
data$hfc_other_check_indicator_name_water_sources_domestic<-as.character("Other Check: Water - Domestic Source")
data$hfc_other_check_comment_water_sources_domestic<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_water_sources_domestic<-data$water_sources_domestic_other
data$hfc_other_check_newvalue_water_sources_domestic<-as.character("")

# Other - Water: Treatment Methods (Select Multiple)
data$hfc_other_check_indicator_water_treatment_methods <- as.integer(0)
data$hfc_other_check_indicator_water_treatment_methods[data$water_treatment_methods.other==1] <- as.integer(1)
data$hfc_other_check_indicator_name_water_treatment_methods<-as.character("Other Check: Water - Treatment Methods")
data$hfc_other_check_comment_water_treatment_methods<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_water_treatment_methods<-data$water_treatment_methods_other
data$hfc_other_check_newvalue_water_treatment_methods<-as.character("")

# Other - Health: Facilities (Select Multiple)
data$hfc_other_check_indicator_health_facilities <- as.integer(0)
data$hfc_other_check_indicator_health_facilities[data$health_facilities.other==1] <- as.integer(1)
data$hfc_other_check_indicator_name_health_facilities<-as.character("Other Check: Health - Facilities")
data$hfc_other_check_comment_health_facilities<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_health_facilities<-data$health_facilities_other
data$hfc_other_check_newvalue_health_facilities<-as.character("")

# Other - Health: Services (Select Multiple)
data$hfc_other_check_indicator_health_services <- as.integer(0)
data$hfc_other_check_indicator_health_services[data$health_services.other==1] <- as.integer(1)
data$hfc_other_check_indicator_name_health_services<-as.character("Other Check: Health - Services")
data$hfc_other_check_comment_health_services<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_health_services<-data$health_services_other
data$hfc_other_check_newvalue_health_services<-as.character("")

# Other - Health: Problems (Select Multiple)
data$hfc_other_check_indicator_health_problems <- as.integer(0)
data$hfc_other_check_indicator_health_problems[data$health_problems.other==1] <- as.integer(1)
data$hfc_other_check_indicator_name_health_problems<-as.character("Other Check: Health - Problems")
data$hfc_other_check_comment_health_problems<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_health_problems<-data$health_problems_other
data$hfc_other_check_newvalue_health_problems<-as.character("")

# Other - Nutrition: Services (Select Multiple)
data$hfc_other_check_indicator_nutrition_services <- as.integer(0)
data$hfc_other_check_indicator_nutrition_services[data$nutrition_services.other==1] <- as.integer(1)
data$hfc_other_check_indicator_name_nutrition_services<-as.character("Other Check: Nutrition - Services")
data$hfc_other_check_comment_nutrition_services<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_nutrition_services<-data$nutrition_services_other
data$hfc_other_check_newvalue_nutrition_services<-as.character("")

# Other - Education: Facilities (Select Multiple)
data$hfc_other_check_indicator_education_facilities <- as.integer(0)
data$hfc_other_check_indicator_education_facilities[data$education_facilities.other==1] <- as.integer(1)
data$hfc_other_check_indicator_name_education_facilities<-as.character("Other Check: Education - Facilities")
data$hfc_other_check_comment_education_facilities<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_education_facilities<-data$education_facilities_other
data$hfc_other_check_newvalue_education_facilities<-as.character("")

# Other - Food Security: Primary Source (Select One)
data$hfc_other_check_indicator_foodsecurity_primary <- as.integer(0)
data$hfc_other_check_indicator_foodsecurity_primary[data$foodsecurity_primary=="other"] <- as.integer(1)
data$hfc_other_check_indicator_name_foodsecurity_primary<-as.character("Other Check: Food Security - Primary Source")
data$hfc_other_check_comment_foodsecurity_primary<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_foodsecurity_primary<-data$foodsecurity_primary_other
data$hfc_other_check_newvalue_foodsecurity_primary<-as.character("")

# Other - Food Security: Secondary Source (Select One)
data$hfc_other_check_indicator_foodsecurity_secondary <- as.integer(0)
data$hfc_other_check_indicator_foodsecurity_secondary[data$foodsecurity_secondary=="other"] <- as.integer(1)
data$hfc_other_check_indicator_name_foodsecurity_secondary<-as.character("Other Check: Food Security - Secondary Source")
data$hfc_other_check_comment_foodsecurity_secondary<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_foodsecurity_secondary<-data$foodsecurity_secondary_other
data$hfc_other_check_newvalue_foodsecurity_secondary<-as.character("")

# Other - Protection: Incidents (Select Multiple)
data$hfc_other_check_indicator_protection_incidents <- as.integer(0)
data$hfc_other_check_indicator_protection_incidents[data$protection_incidents.other==1] <- as.integer(1)
data$hfc_other_check_indicator_name_protection_incidents<-as.character("Other Check: Protection - Incidents")
data$hfc_other_check_comment_protection_incidents<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_protection_incidents<-data$protection_incidents_other
data$hfc_other_check_newvalue_protection_incidents<-as.character("")

# Other - Support: Activities (Select Multiple)
data$hfc_other_check_indicator_support <- as.integer(0)
data$hfc_other_check_indicator_support[data$support.other==1] <- as.integer(1)
data$hfc_other_check_indicator_name_support<-as.character("Other Check: Support - Activities")
data$hfc_other_check_comment_support<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_support<-data$support_other
data$hfc_other_check_newvalue_support<-as.character("")

# Other - AAP: Information Sources (Select Multiple)
data$hfc_other_check_indicator_aap_informationsources <- as.integer(0)
data$hfc_other_check_indicator_aap_informationsources[data$aap_informationsources.other==1] <- as.integer(1)
data$hfc_other_check_indicator_name_aap_informationsources<-as.character("Other Check: AAP - Information Sources")
data$hfc_other_check_comment_aap_informationsources<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_aap_informationsources<-data$aap_informationsources_other
data$hfc_other_check_newvalue_aap_informationsources<-as.character("")

# Other - AAP: Information Sources for Persons with Difficulties (Select Multiple)
data$hfc_other_check_indicator_aap_informationsources_pwd <- as.integer(0)
data$hfc_other_check_indicator_aap_informationsources_pwd[data$aap_informationsources_pwd.other==1] <- as.integer(1)
data$hfc_other_check_indicator_name_aap_informationsources_pwd<-as.character("Other Check: AAP - Information Sources for Persons with Difficulties")
data$hfc_other_check_comment_aap_informationsources_pwd<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_aap_informationsources_pwd<-data$aap_informationsources_pwd_other
data$hfc_other_check_newvalue_aap_informationsources_pwd<-as.character("")

# Other - AAP: Information Sources for Persons with Difficulties (Select Multiple)
data$hfc_other_check_indicator_aap_humanitarianassistanceproblems <- as.integer(0)
data$hfc_other_check_indicator_aap_humanitarianassistanceproblems[data$aap_humanitarianassistanceproblems.other==1] <- as.integer(1)
data$hfc_other_check_indicator_name_aap_humanitarianassistanceproblems<-as.character("Other Check: AAP - Humanitarian Assistance Problems")
data$hfc_other_check_comment_aap_humanitarianassistanceproblems<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_aap_humanitarianassistanceproblems<-data$aap_humanitarianassistanceproblems_other
data$hfc_other_check_newvalue_aap_humanitarianassistanceproblems<-as.character("")

# Other - Key Informant Status (Select One)
data$hfc_other_check_indicator_ki_status <- as.integer(0)
data$hfc_other_check_indicator_ki_status[data$ki_status=="other"] <- as.integer(1)
data$hfc_other_check_indicator_name_ki_status<-as.character("Other Check: Key Informant - Status")
data$hfc_other_check_comment_ki_status<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_ki_status<-data$ki_status_other
data$hfc_other_check_newvalue_ki_status<-as.character("")

# Other - Referral Key Informant Status (Select One)
data$hfc_other_check_indicator_ki_referral_status <- as.integer(0)
data$hfc_other_check_indicator_ki_referral_status[data$ki_referral_status=="other"] <- as.integer(1)
data$hfc_other_check_indicator_name_ki_referral_status<-as.character("Other Check: Referral Key Informant - Status")
data$hfc_other_check_comment_ki_referral_status<-as.character(paste0("Explain other response"))
data$hfc_other_check_oldvalue_ki_referral_status<-data$ki_referral_status_other
data$hfc_other_check_newvalue_ki_referral_status<-as.character("")

list_indicators_other_check <- c("cccm_management", 
                                 "cccm_committees", 
                                 "evictions_landowner", 
                                 "evictions_tenureagreement_holder", 
                                 "evictions_tenureagreement_rentpayment",
                                 "water_sources_primary", 
                                 "water_sources_secondary", 
                                 "water_sources_domestic", 
                                 "water_treatment_methods",
                                 "health_facilities", 
                                 "health_services", 
                                 "health_problems", 
                                 "nutrition_services", 
                                 "education_facilities", 
                                 "foodsecurity_primary", 
                                 "foodsecurity_secondary", 
                                 "protection_incidents",
                                 "support", 
                                 "aap_informationsources", 
                                 "aap_informationsources_pwd", 
                                 "aap_humanitarianassistanceproblems", 
                                 "ki_status", 
                                 "ki_referral_status")

for (i in list_indicators_other_check){
  temp_name <- paste0("hfc_other_check_", i)
  temp_name_indicator <- paste0("hfc_other_check_indicator_", i)
  temp_name_indicator_name <- paste0("hfc_other_check_indicator_name_", i)
  temp_name_comment <- paste0("hfc_other_check_comment_", i)
  temp_name_oldvalue <- paste0("hfc_other_check_oldvalue_", i)
  temp_name_newvalue <- paste0("hfc_other_check_newvalue_", i)
  temp_name <- data.frame(data$X_uuid, data$localisation_region, data$localisation_district, data$localisation_settlement_name_local, data$deviceid, data[temp_name_indicator], data[temp_name_indicator_name], data[temp_name_comment], data[temp_name_oldvalue], data[temp_name_newvalue])
  temp_name <- subset(temp_name, data[temp_name_indicator]!=0)
  names(temp_name)[names(temp_name) == 'data.X_uuid'] <- 'UUID'
  names(temp_name)[names(temp_name) == 'data.localisation_region'] <- 'Region'
  names(temp_name)[names(temp_name) == 'data.localisation_district'] <- 'District'
  names(temp_name)[names(temp_name) == 'data.localisation_settlement_name_local'] <- 'Settlement'
  names(temp_name)[names(temp_name) == 'data.deviceid'] <- 'Enumerator'
  names(temp_name)[names(temp_name) == paste0("hfc_other_check_indicator_name_", i)] <- 'Indicator'
  names(temp_name)[names(temp_name) == paste0("hfc_other_check_comment_", i)] <- 'Comment'
  names(temp_name)[names(temp_name) == paste0("hfc_other_check_oldvalue_", i)] <- 'OldValue'
  names(temp_name)[names(temp_name) == paste0("hfc_other_check_newvalue_", i)] <- 'NewValue'
  temp_name <- data.frame(temp_name$UUID, temp_name$Region, temp_name$District, temp_name$Settlement, temp_name$Enumerator, temp_name$Indicator, temp_name$Comment, temp_name$OldValue, temp_name$NewValue)
  names(temp_name)[names(temp_name) == 'temp_name.UUID'] <- 'UUID'
  names(temp_name)[names(temp_name) == 'temp_name.Region'] <- 'Region'
  names(temp_name)[names(temp_name) == 'temp_name.District'] <- 'District'
  names(temp_name)[names(temp_name) == 'temp_name.Settlement'] <- 'Settlement'
  names(temp_name)[names(temp_name) == 'temp_name.Enumerator'] <- 'Enumerator'
  names(temp_name)[names(temp_name) == paste0("temp_name.Indicator")] <- 'Indicator'
  names(temp_name)[names(temp_name) == paste0("temp_name.Comment")] <- 'Comment'
  names(temp_name)[names(temp_name) == paste0("temp_name.OldValue")] <- 'OldValue'
  names(temp_name)[names(temp_name) == paste0("temp_name.NewValue")] <- 'NewValue'
  dsa_hfc_issues_log <- rbind(dsa_hfc_issues_log, temp_name)
}

# 2.11    Check that date values fall within survey range
#dsa_datacollection <- interval(ymd("2019-11-17"), ymd("2019-12-31"))

# 2.12    Check that there are no outliers for unconstrained variables
data$hfc_outlier_check_indicator_name_cccm_populationestimates_shelters<-as.character("Outlier Check: Number of Shelters")
data$hfc_outlier_check_comment_cccm_populationestimates_shelters<-as.character(paste0("Number of shelters provided, ", data$cccm_populationestimates_shelters, ", is an outlier. Please confirm in the Remote KII."))
data$hfc_outlier_check_indicator_name_cccm_populationestimates_families<-as.character("Outlier Check: Number of Families")
data$hfc_outlier_check_comment_cccm_populationestimates_families<-as.character(paste0("Number of families provided, ", data$cccm_populationestimates_families, ", is an outlier. Please confirm in the Remote KII."))
data$hfc_outlier_check_indicator_name_cccm_populationestimates_individuals<-as.character("Outlier Check: Number of Individuals")
data$hfc_outlier_check_comment_cccm_populationestimates_individuals<-as.character(paste0("Number of individuals provided, ", data$cccm_populationestimates_individuals, ", is an outlier. Please confirm in the Remote KII."))
data$hfc_outlier_check_indicator_name_cccm_site_duration<-as.character("Outlier Check: Site Duration")
data$hfc_outlier_check_comment_cccm_site_duration<-as.character(paste0("Number of months that site has been established, ", data$cccm_site_duration, ", is an outlier. Please confirm in the Remote KII."))
data$hfc_outlier_check_indicator_name_cccm_idps_arrived<-as.character("Outlier Check: IDPs Arrived")
data$hfc_outlier_check_comment_cccm_idps_arrived<-as.character(paste0("Number of IDP families arrived in the past 3 months, ", data$cccm_idps_arrived, ", is an outlier. Please confirm in the Remote KII."))
data$hfc_outlier_check_indicator_name_cccm_idps_departed<-as.character("Outlier Check: IDPs Departed")
data$hfc_outlier_check_comment_cccm_idps_departed<-as.character(paste0("Number of IDP families departed in the past 3 months, ", data$cccm_idps_departed, ", is an outlier. Please confirm in the Remote KII."))
data$hfc_outlier_check_indicator_name_evictions_households<-as.character("Outlier Check: Households Evicted")
data$hfc_outlier_check_comment_evictions_households<-as.character(paste0("Number of households evicted, ", data$evictions_households, ", is an outlier. Please confirm in the Remote KII."))

data$hfc_outlier_check_indicator_name_shelter_fire_destroyed<-as.character("Outlier Check: Houses destroyed by Fire")
data$hfc_outlier_check_comment_shelter_fire_destroyed<-as.character(paste0("Number of houses destroyed by fire, ", data$shelter_fire_destroyed, ", is an outlier. Please confirm in the Remote KII."))
data$hfc_outlier_check_indicator_name_shelter_flood_destroyed<-as.character("Outlier Check: Houses destroyed by Flood")
data$hfc_outlier_check_comment_shelter_flood_destroyed<-as.character(paste0("Number of houses destroyed by flood, ", data$shelter_flood_destroyed, ", is an outlier. Please confirm in the Remote KII."))

data$hfc_outlier_check_indicator_name_nfi_access_distance_min<-as.character("Outlier Check: Minimum Access Distance")
data$hfc_outlier_check_comment_nfi_access_distance_min<-as.character(paste0("Minimum distance (in minutes) to NFI markets is, ", data$nfi_access_distance_min, ", is an outlier. Please confirm in the Remote KII."))
data$hfc_outlier_check_indicator_name_nfi_access_distance_max<-as.character("Outlier Check: Maximum Access Distance")
data$hfc_outlier_check_comment_nfi_access_distance_max<-as.character(paste0("Maximum distance (in minutes) to NFI markets is, ", data$nfi_access_distance_max, ", is an outlier. Please confirm in the Remote KII."))

data$hfc_outlier_check_indicator_name_water_access_distance_min<-as.character("Outlier Check: Minimum Access Distance")
data$hfc_outlier_check_comment_water_access_distance_min<-as.character(paste0("Minimum distance (in minutes) to water sources is, ", data$water_access_distance_min, ", is an outlier. Please confirm in the Remote KII."))
data$hfc_outlier_check_indicator_name_water_access_distance_max<-as.character("Outlier Check: Maximum Access Distance")
data$hfc_outlier_check_comment_water_access_distance_max<-as.character(paste0("Maximum distance (in minutes) to water sources is, ", data$water_access_distance_max, ", is an outlier. Please confirm in the Remote KII."))

data$hfc_outlier_check_indicator_name_sanitation_access_distance_min<-as.character("Outlier Check: Minimum Access Distance")
data$hfc_outlier_check_comment_sanitation_access_distance_min<-as.character(paste0("Minimum distance (in minutes) to latrines is, ", data$sanitation_access_distance_min, ", is an outlier. Please confirm in the Remote KII."))
data$hfc_outlier_check_indicator_name_sanitation_access_distance_max<-as.character("Outlier Check: Maximum Access Distance")
data$hfc_outlier_check_comment_sanitation_access_distance_max<-as.character(paste0("Maximum distance (in minutes) to latrines is, ", data$sanitation_access_distance_max, ", is an outlier. Please confirm in the Remote KII."))

data$hfc_outlier_check_indicator_name_hygiene_access_distance_min<-as.character("Outlier Check: Minimum Access Distance")
data$hfc_outlier_check_comment_hygiene_access_distance_min<-as.character(paste0("Minimum distance (in minutes) to bathing facilities is, ", data$hygiene_access_distance_min, ", is an outlier. Please confirm in the Remote KII."))
data$hfc_outlier_check_indicator_name_hygiene_access_distance_max<-as.character("Outlier Check: Maximum Access Distance")
data$hfc_outlier_check_comment_hygiene_access_distance_max<-as.character(paste0("Maximum distance (in minutes) to bathing facilities is, ", data$hygiene_access_distance_max, ", is an outlier. Please confirm in the Remote KII."))

data$hfc_outlier_check_indicator_name_health_access_distance_min<-as.character("Outlier Check: Minimum Access Distance")
data$hfc_outlier_check_comment_health_access_distance_min<-as.character(paste0("Minimum distance (in minutes) to health facilities is, ", data$health_access_distance_min, ", is an outlier. Please confirm in the Remote KII."))
data$hfc_outlier_check_indicator_name_health_access_distance_max<-as.character("Outlier Check: Maximum Access Distance")
data$hfc_outlier_check_comment_health_access_distance_max<-as.character(paste0("Maximum distance (in minutes) to health facilities is, ", data$health_access_distance_max, ", is an outlier. Please confirm in the Remote KII."))

data$hfc_outlier_check_indicator_name_nutrition_access_distance_min<-as.character("Outlier Check: Minimum Access Distance")
data$hfc_outlier_check_comment_nutrition_access_distance_min<-as.character(paste0("Minimum distance (in minutes) to nutrition services is, ", data$nutrition_access_distance_min, ", is an outlier. Please confirm in the Remote KII."))
data$hfc_outlier_check_indicator_name_nutrition_access_distance_max<-as.character("Outlier Check: Maximum Access Distance")
data$hfc_outlier_check_comment_nutrition_access_distance_max<-as.character(paste0("Maximum distance (in minutes) to nutrition services is, ", data$nutrition_access_distance_max, ", is an outlier. Please confirm in the Remote KII."))

data$hfc_outlier_check_indicator_name_education_access_distance_min<-as.character("Outlier Check: Minimum Access Distance")
data$hfc_outlier_check_comment_education_access_distance_min<-as.character(paste0("Minimum distance (in minutes) to education facilities is, ", data$education_access_distance_min, ", is an outlier. Please confirm in the Remote KII."))
data$hfc_outlier_check_indicator_name_education_access_distance_max<-as.character("Outlier Check: Maximum Access Distance")
data$hfc_outlier_check_comment_education_access_distance_max<-as.character(paste0("Maximum distance (in minutes) to education facilities  is, ", data$education_access_distance_max, ", is an outlier. Please confirm in the Remote KII."))

data$hfc_outlier_check_indicator_name_foodsecurity_access_distance_min<-as.character("Outlier Check: Minimum Access Distance")
data$hfc_outlier_check_comment_foodsecurity_access_distance_min<-as.character(paste0("Minimum distance (in minutes) to food markets is, ", data$foodsecurity_access_distance_min, ", is an outlier. Please confirm in the Remote KII."))
data$hfc_outlier_check_indicator_name_foodsecurity_access_distance_max<-as.character("Outlier Check: Maximum Access Distance")
data$hfc_outlier_check_comment_foodsecurity_access_distance_max<-as.character(paste0("Maximum distance (in minutes) to food markets is, ", data$foodsecurity_access_distance_max, ", is an outlier. Please confirm in the Remote KII."))

data$hfc_outlier_check_indicator_name_water_sources_present<-as.character("Outlier Check: Water Sources Present")
data$hfc_outlier_check_comment_water_sources_present<-as.character(paste0("Number of water sources present, ", data$water_sources_present, ", is an outlier. Please confirm in the Remote KII."))

data$hfc_outlier_check_indicator_name_sanitation_toilets_male<-as.character("Outlier Check: Male Toilets")
data$hfc_outlier_check_comment_sanitation_toilets_male<-as.character(paste0("Number of male toilets, ", data$sanitation_toilets_male, ", is an outlier. Please confirm in the Remote KII."))
data$hfc_outlier_check_indicator_name_sanitation_toilets_female<-as.character("Outlier Check: Female Toilets")
data$hfc_outlier_check_comment_sanitation_toilets_female<-as.character(paste0("Number of female toilets, ", data$sanitation_toilets_female, ", is an outlier. Please confirm in the Remote KII."))
data$hfc_outlier_check_indicator_name_sanitation_toilets_nongendered<-as.character("Outlier Check: Non-Gendered Toilets")
data$hfc_outlier_check_comment_sanitation_toilets_nongendered<-as.character(paste0("Number of nongendered toilets, ", data$sanitation_toilets_nongendered, ", is an outlier. Please confirm in the Remote KII."))
data$hfc_outlier_check_indicator_name_hygiene_bathingfacilities<-as.character("Outlier Check: Bathing Facilities")
data$hfc_outlier_check_comment_hygiene_bathingfacilities<-as.character(paste0("Number of bathing facilities, ", data$hygiene_bathingfacilities, ", is an outlier. Please confirm in the Remote KII."))

list_outlier_checks<-c("cccm_populationestimates_shelters",
                       "cccm_populationestimates_families",
                       "cccm_populationestimates_individuals",
                       "cccm_site_duration",
                       "cccm_idps_arrived",
                       "cccm_idps_departed",
                       "evictions_households",
                       "shelter_fire_destroyed",
                       "shelter_flood_destroyed",
                       "nfi_access_distance_min",
                       "nfi_access_distance_max",
                       "water_sources_present",
                       "water_access_distance_min",
                       "water_access_distance_max",
                       "sanitation_toilets_male",
                       "sanitation_toilets_female",
                       "sanitation_toilets_nongendered",
                       "hygiene_bathingfacilities",
                       "sanitation_access_distance_min",
                       "sanitation_access_distance_max",
                       "hygiene_access_distance_min",
                       "hygiene_access_distance_max",
                       "health_access_distance_min",
                       "health_access_distance_max",
                       "nutrition_access_distance_min",
                       "nutrition_access_distance_max",
                       "education_access_distance_min",
                       "education_access_distance_max",
                       "foodsecurity_access_distance_min",
                       "foodsecurity_access_distance_max"
)

for (i in list_outlier_checks){
  temp_name <- paste0(i)
  temp_name_outlier_values <- paste0("hfc_outlier_values_", i)
  temp_name_hfc_verification_check <- paste0("hfc_outlier_check_indicator_", i)
  temp_name_oldvalue <- paste0("hfc_outlier_check_oldvalue_", i)
  temp_name_newvalue <- paste0("hfc_outlier_check_newvalue_", i)
  data <- data[order(data[[temp_name]]),]
  temp_name_outlier_values <- boxplot.stats(data[[temp_name]])$out 
  data[[temp_name_hfc_verification_check]]<-as.integer(0)
  data[[temp_name_hfc_verification_check]][data[[temp_name]] %in% temp_name_outlier_values]<-as.integer(1)
  data[[temp_name_oldvalue]]<-as.character("")
  data[[temp_name_newvalue]]<-as.character("")
  }


for (i in list_outlier_checks){
  temp_name <- paste0("hfc_outlier_check_", i)
  temp_name_indicator <- paste0("hfc_outlier_check_indicator_", i)
  temp_name_indicator_name <- paste0("hfc_outlier_check_indicator_name_", i)
  temp_name_comment <- paste0("hfc_outlier_check_comment_", i)
  temp_name_oldvalue <- paste0("hfc_outlier_check_oldvalue_", i)
  temp_name_newvalue <- paste0("hfc_outlier_check_newvalue_", i)
  temp_name <- data.frame(data$X_uuid, data$localisation_region, data$localisation_district, data$localisation_settlement_name_local, data$deviceid, data[temp_name_indicator], data[temp_name_indicator_name], data[temp_name_comment], data[temp_name_oldvalue], data[temp_name_newvalue])
  temp_name <- subset(temp_name, data[temp_name_indicator]!=0)
  names(temp_name)[names(temp_name) == 'data.X_uuid'] <- 'UUID'
  names(temp_name)[names(temp_name) == 'data.localisation_region'] <- 'Region'
  names(temp_name)[names(temp_name) == 'data.localisation_district'] <- 'District'
  names(temp_name)[names(temp_name) == 'data.localisation_settlement_name_local'] <- 'Settlement'
  names(temp_name)[names(temp_name) == 'data.deviceid'] <- 'Enumerator'
  names(temp_name)[names(temp_name) == paste0("hfc_outlier_check_indicator_name_", i)] <- 'Indicator'
  names(temp_name)[names(temp_name) == paste0("hfc_outlier_check_comment_", i)] <- 'Comment'
  names(temp_name)[names(temp_name) == paste0("hfc_outlier_check_oldvalue_", i)] <- 'OldValue'
  names(temp_name)[names(temp_name) == paste0("hfc_outlier_check_newvalue_", i)] <- 'NewValue'
  temp_name <- data.frame(temp_name$UUID, temp_name$Region, temp_name$District, temp_name$Settlement, temp_name$Enumerator, temp_name$Indicator, temp_name$Comment, temp_name$OldValue, temp_name$NewValue)
  names(temp_name)[names(temp_name) == 'temp_name.UUID'] <- 'UUID'
  names(temp_name)[names(temp_name) == 'temp_name.Region'] <- 'Region'
  names(temp_name)[names(temp_name) == 'temp_name.District'] <- 'District'
  names(temp_name)[names(temp_name) == 'temp_name.Settlement'] <- 'Settlement'
  names(temp_name)[names(temp_name) == 'temp_name.Enumerator'] <- 'Enumerator'
  names(temp_name)[names(temp_name) == paste0("temp_name.Indicator")] <- 'Indicator'
  names(temp_name)[names(temp_name) == paste0("temp_name.Comment")] <- 'Comment'
  names(temp_name)[names(temp_name) == paste0("temp_name.OldValue")] <- 'OldValue'
  names(temp_name)[names(temp_name) == paste0("temp_name.NewValue")] <- 'NewValue'
  dsa_hfc_issues_log <- rbind(dsa_hfc_issues_log, temp_name)
}

# 2.13    Compile all field comments
# NOT REQUIRED
# 2.14    Check SurveyCTO text audit fields for duration per question

# 3.0   Enumerator Summary
dsa_hfc_enumerator_summary <- summarise(group_by(data, deviceid, localisation_region),count =n())
list_check_survey_tracking <- unique(na.omit(data$today))
for(j in list_check_survey_tracking){
  dsa_hfc_enumerator_summary[, ncol(dsa_hfc_enumerator_summary) + 1] <- (data %>%
                                                                           group_by(deviceid, localisation_region) %>%
                                                                           summarise(Val=sum(na.omit(today==j))) %>%
                                                                           arrange(deviceid, localisation_region)) $Val 
  names(dsa_hfc_enumerator_summary)[ncol(dsa_hfc_enumerator_summary)] <- paste0("Date: ", j)
}

names(dsa_hfc_enumerator_summary)[names(dsa_hfc_enumerator_summary) == 'localisation_region'] <- 'Region'
names(dsa_hfc_enumerator_summary)[names(dsa_hfc_enumerator_summary) == 'count'] <- 'TotalNumberOfSurveys'

dsa_hfc_enumerator_summary[, ncol(dsa_hfc_enumerator_summary) + 1] <- (data %>%
                                                                         group_by(deviceid, localisation_region) %>%
                                                                         summarise(Val=trunc(mean(survey_duration, na.rm=TRUE))) %>%
                                                                         arrange(deviceid, localisation_region))$Val
names(dsa_hfc_enumerator_summary)[ncol(dsa_hfc_enumerator_summary)] <- paste0("Average Survey Duration")

# 3.01  Check the percentage of "don't know" and "refusal" values for each variable by enumerator
list_indicators_response_check_dontknow <- c("evictions_landowner",
                                             "evictions_tenureagreement_holder.donotknow",
                                             "evictions_tenureagreement_renewal",
                                             "evictions_tenureagreement_rentpayment",
                                             "evictions_tenureagreement_rentfrequency",
                                             "water_treatment_methods.donotknow",
                                             "sanitation_desludging",
                                             "sanitation_solidwastedisposal",
                                             "aap_informationsources.donotknow",
                                             "aap_informationsources_pwd.donotknow") 
data$hfc_response_check_dontknow_total<-as.integer(0)
data$hfc_response_check_dontknow_question_total<-as.integer(0)
for (i in list_indicators_response_check_dontknow){
  temp_name <- paste0("hfc_response_check_dontknow_", i)
  temp_name_question <- paste0("hfc_response_check_dontknow_question", i)
  temp_name_variable <- paste0(i)
  data[[temp_name]] <- as.integer(0)
  data[[temp_name_question]] <- as.integer(1)
  data[[temp_name]][data[[temp_name_variable]]=="donotknow"] <- as.integer(1)
  data[[temp_name]][data[[temp_name_variable]]==as.integer(1)] <- as.integer(1)
  data$hfc_response_check_dontknow_total<-data$hfc_response_check_dontknow_total+data[[temp_name]]
  data$hfc_response_check_dontknow_question_total<-data$hfc_response_check_dontknow_question_total+data[[temp_name_question]]
}

dsa_hfc_enumerator_summary[, ncol(dsa_hfc_enumerator_summary) + 1] <- 100*(data %>%
                                                                             group_by(deviceid, localisation_region) %>%
                                                                             summarise(Val=sum(na.omit(hfc_response_check_dontknow_total))) %>%
                                                                             arrange(deviceid, localisation_region)) $Val / (data %>%
                                                                                                                                      group_by(deviceid, localisation_region) %>%
                                                                                                                                      summarize(Nbr=sum(na.omit(hfc_response_check_dontknow_question_total))) %>%
                                                                                                                                      arrange(deviceid, localisation_region))$Nbr 
names(dsa_hfc_enumerator_summary)[ncol(dsa_hfc_enumerator_summary)] <- paste0("Response: Do Not Know")
dsa_hfc_enumerator_summary$`Response: Do Not Know`<-paste(round(dsa_hfc_enumerator_summary$`Response: Do Not Know`,digits=0),"%",sep="")

list_indicators_response_check_noanswer <- c("shelter_types.no_answer",
                                             "nfi_items_available.no_answer",
                                             "water_sources_primary",
                                             "water_sources_secondary",
                                             "water_sources_domestic",
                                             "water_treatment_methods.other",
                                             "sanitation_solidwastedisposal",
                                             "health_facilities.other",
                                             "health_services.other",
                                             "health_problems.other",
                                             "nutrition_services.other",
                                             "nutrition_distributions.other",
                                             "education_facilities.other",
                                             "foodsecurity_secondary",
                                             "foodsecurity_livelihood",
                                             "protection_incidents.other",
                                             "protection_incidents_type.no_answer",
                                             "support_activities.other",
                                             "aap_informationsources.other",
                                             "aap_informationsources_pwd.other",
                                             "aap_humanitarianassistanceproblems.other",
                                             "aap_languages.other",
                                             "nfi_access_impediments.no_answer",
                                             "water_access_impediments.no_answer",
                                             "sanitation_access_impediments.no_answer",
                                             "hygiene_access_impediments.no_answer",
                                             "health_access_impediments.no_answer",
                                             "nutrition_access_impediments.no_answer",
                                             "education_access_impediments.no_answer",
                                             "foodsecurity_access_impediments.no_answer",
                                             "support_access_impediments.no_answer",
                                             "aap_access_impediments.no_answer")
data$hfc_response_check_noanswer_total<-as.integer(0)
data$hfc_response_check_noanswer_question_total<-as.integer(0)
for (i in list_indicators_response_check_noanswer){
  temp_name <- paste0("hfc_response_check_noanswer_", i)
  temp_name_question <- paste0("hfc_response_check_noanswer_question", i)
  temp_name_variable <- paste0(i)
  data[[temp_name]] <- as.integer(0)
  data[[temp_name_question]] <- as.integer(1)
  data[[temp_name]][data[[temp_name_variable]]=="donotknow"] <- as.integer(1)
  data[[temp_name]][data[[temp_name_variable]]==as.integer(1)] <- as.integer(1)
  data$hfc_response_check_noanswer_total<-data$hfc_response_check_noanswer_total+data[[temp_name]]
  data$hfc_response_check_noanswer_question_total<-data$hfc_response_check_noanswer_question_total+data[[temp_name_question]]
}

dsa_hfc_enumerator_summary[, ncol(dsa_hfc_enumerator_summary) + 1] <- 100*(data %>%
                                                                             group_by(deviceid, localisation_region) %>%
                                                                             summarise(Val=sum(na.omit(hfc_response_check_noanswer_total))) %>%
                                                                             arrange(deviceid, localisation_region)) $Val / (data %>%
                                                                                                                                      group_by(deviceid, localisation_region) %>%
                                                                                                                                      summarize(Nbr=sum(na.omit(hfc_response_check_noanswer_question_total))) %>%
                                                                                                                                      arrange(deviceid, localisation_region))$Nbr 
names(dsa_hfc_enumerator_summary)[ncol(dsa_hfc_enumerator_summary)] <- paste0("Response: No Answer")
dsa_hfc_enumerator_summary$`Response: No Answer`<-paste(round(dsa_hfc_enumerator_summary$`Response: No Answer`,digits=0),"%",sep="")

# 3.02  Check the percentage giving each answer for key filter questions by enumerator
# NOT REQUIRED
# 3.03  Check the percentage of survey refusals by enumerator
# NOT REQUIRED
# 3.04  Check the number of surveys per day by enumerator
# DONE
# 3.05  Check average interview duration by enumerator
# DONE
# 3.06  Check the duration of consent and other important questions (anthropometrics, games, etc) by enumerator
# NOT REQUIRED
# 3.07  Check the percentage of choosing "other" response by enumerator
list_indicators_response_check_other <- c("ki_status",
                                          "localisation_region",
                                          "localisation_district",
                                          "localisation_settlement",
                                          "cccm_management.other",
                                          "cccm_committees.other",
                                          "evictions_landowner",
                                          "evictions_tenureagreement_holder.other",
                                          "evictions_tenureagreement_rentpayment",
                                          "water_sources_primary",
                                          "water_sources_secondary",
                                          "water_sources_domestic",
                                          "water_treatment_methods.other",
                                          "sanitation_solidwastedisposal",
                                          "health_facilities.other",
                                          "health_services.other",
                                          "health_problems.other",
                                          "nutrition_services.other",
                                          "education_facilities.other",
                                          "foodsecurity_source_primary",
                                          "foodsecurity_source_secondary",
                                          "protection_incidents.other",
                                          "support_activities.other",
                                          "aap_informationsources_pwd.other",
                                          "aap_humanitarianassistanceproblems.other",
                                          "aap_languages.other",
                                          "settlements") 
data$hfc_response_check_other_total<-as.integer(0)
data$hfc_response_check_other_question_total<-as.integer(0)
for (i in list_indicators_response_check_other){
  temp_name <- paste0("hfc_response_check_other_", i)
  temp_name_question <- paste0("hfc_response_check_other_question", i)
  temp_name_variable <- paste0(i)
  data[[temp_name]] <- as.integer(0)
  data[[temp_name_question]] <- as.integer(1)
  data[[temp_name]][data[[temp_name_variable]]=="other"] <- as.integer(1)
  data[[temp_name]][data[[temp_name_variable]]==as.integer(1)] <- as.integer(1)
  data$hfc_response_check_other_total<-data$hfc_response_check_other_total+data[[temp_name]]
  data$hfc_response_check_other_question_total<-data$hfc_response_check_other_question_total+data[[temp_name_question]]
}

dsa_hfc_enumerator_summary[, ncol(dsa_hfc_enumerator_summary) + 1] <- 100*(data %>%
                                                                             group_by(deviceid, localisation_region) %>%
                                                                             summarise(Val=sum(na.omit(hfc_response_check_other_total))) %>%
                                                                             arrange(deviceid, localisation_region)) $Val / (data %>%
                                                                                                                                      group_by(deviceid, localisation_region) %>%
                                                                                                                                      summarize(Nbr=sum(na.omit(hfc_response_check_other_question_total))) %>%
                                                                                                                                      arrange(deviceid, localisation_region))$Nbr 
names(dsa_hfc_enumerator_summary)[ncol(dsa_hfc_enumerator_summary)] <- paste0("Response: Other")
dsa_hfc_enumerator_summary$`Response: Other`<-paste(round(dsa_hfc_enumerator_summary$`Response: Other`,digits=0),"%",sep="")

list_response_checks <- c("ki_resident",
                          "ki_status",
                          "cccm_site_capacity",
                          "cccm_idps_arrival",
                          "cccm_idps_departure",
                          "cccm_management.cccm_management_1",
                          "cccm_management.cccm_management_2",
                          "cccm_management.cccm_management_3",
                          "cccm_management.cccm_management_4",
                          "cccm_management.cccm_management_5",
                          "cccm_management.cccm_management_6",
                          "cccm_management.cccm_management_7",
                          "cccm_management.cccm_management_8",
                          "cccm_committees.cccm_committees_1",
                          "cccm_committees.cccm_committees_2",
                          "cccm_committees.cccm_committees_3",
                          "cccm_committees.cccm_committees_4",
                          "cccm_committees.cccm_committees_5",
                          "cccm_committees.cccm_committees_6",
                          "cccm_committees.cccm_committees_7",
                          "cccm_committees.cccm_committees_8",
                          "cccm_committees_women",
                          "evictions_landowner",
                          "evictions_tenureagreement",
                          "evictions_tenureagreement_form",
                          "evictions_tenureagreement_holder.evictions_tenureagreement_holder_1",
                          "evictions_tenureagreement_holder.evictions_tenureagreement_holder_2",
                          "evictions_tenureagreement_holder.evictions_tenureagreement_holder_3",
                          "evictions_tenureagreement_holder.evictions_tenureagreement_holder_4",
                          "evictions_tenureagreement_holder.evictions_tenureagreement_holder_5",
                          "evictions_tenureagreement_holder.evictions_tenureagreement_holder_6",
                          "evictions_tenureagreement_renewal",
                          "evictions_tenureagreement_rentpayment",
                          "evictions_tenureagreement_rentfrequency",
                          "evictions_notice",
                          "evictions_notice_leavedate",
                          "shelter_types.shelter_types_1",
                          "shelter_types.shelter_types_2",
                          "shelter_types.shelter_types_3",
                          "shelter_types.shelter_types_4",
                          "shelter_types.shelter_types_5",
                          "shelter_types.shelter_types_6",
                          "shelter_types.shelter_types_7",
                          "shelter_types.shelter_types_8",
                          "shelter_fire",
                          "shelter_flood",
                          "shelter_publiclighting",
                          "nfi_access",
                          "nfi_items_available.nfi_item_1",
                          "nfi_items_available.nfi_item_2",
                          "nfi_items_available.nfi_item_3",
                          "nfi_items_available.nfi_item_4",
                          "nfi_items_available.nfi_item_5",
                          "nfi_items_available.nfi_item_6",
                          "nfi_items_available.nfi_item_7",
                          "nfi_items_available.nfi_item_8",
                          "nfi_items_available.nfi_item_9",
                          "nfi_items_available.nfi_item_10",
                          "nfi_items_available.nfi_item_11",
                          "nfi_items_available.nfi_item_12",
                          "nfi_items_available.nfi_item_13",
                          "nfi_items_available.nfi_item_14",
                          "nfi_access_impediments.impediments_populationgroups_1",
                          "nfi_access_impediments.impediments_populationgroups_2",
                          "nfi_access_impediments.impediments_populationgroups_3",
                          "nfi_access_impediments.impediments_populationgroups_4",
                          "nfi_access_impediments.impediments_populationgroups_5",
                          "water_sources_functional",
                          "water_sources_primary",
                          "water_sources_secondary",
                          "water_sources_domestic",
                          "water_treatment_methods",
                          "water_treatment_methods.water_treatment_1",
                          "water_treatment_methods.water_treatment_2",
                          "water_treatment_methods.water_treatment_3",
                          "water_treatment_methods.water_treatment_4",
                          "water_access_impediments.impediments_populationgroups_1",
                          "water_access_impediments.impediments_populationgroups_2",
                          "water_access_impediments.impediments_populationgroups_3",
                          "water_access_impediments.impediments_populationgroups_4",
                          "water_access_impediments.impediments_populationgroups_5",
                          "hygiene_handwashingfacilities",	
                          "sanitation_lockabletoilets",
                          "sanitation_toiletlighting",	
                          "sanitation_desludging",	
                          "sanitation_faecalmatter",
                          "sanitation_solidwastedisposal",
                          "sanitation_access_impediments.impediments_populationgroups_1",
                          "sanitation_access_impediments.impediments_populationgroups_2",
                          "sanitation_access_impediments.impediments_populationgroups_3",
                          "sanitation_access_impediments.impediments_populationgroups_4",
                          "sanitation_access_impediments.impediments_populationgroups_5",
                          "hygiene_access_impediments.impediments_populationgroups_1",
                          "hygiene_access_impediments.impediments_populationgroups_2",
                          "hygiene_access_impediments.impediments_populationgroups_3",
                          "hygiene_access_impediments.impediments_populationgroups_4",
                          "hygiene_access_impediments.impediments_populationgroups_5",
                          "health_facilities.health_facilities_1",
                          "health_facilities.health_facilities_2",
                          "health_facilities.health_facilities_3",
                          "health_facilities.health_facilities_4",
                          "health_facilities.health_facilities_5",
                          "health_facilities.health_facilities_6",
                          "health_facilities.health_facilities_7",
                          "health_services.health_services_1",
                          "health_services.health_services_2",
                          "health_services.health_services_3",
                          "health_services.health_services_4",
                          "health_services.health_services_5",
                          "health_services.health_services_6",
                          "health_services.health_services_7",
                          "health_problems.health_problems_1",
                          "health_problems.health_problems_2",
                          "health_problems.health_problems_3",
                          "health_problems.health_problems_4",
                          "health_problems.health_problems_5",
                          "health_problems.health_problems_6",
                          "health_problems.health_problems_7",
                          "health_problems.health_problems_8",
                          "hygiene_access_impediments.impediments_populationgroups_1",
                          "hygiene_access_impediments.impediments_populationgroups_2",
                          "hygiene_access_impediments.impediments_populationgroups_3",
                          "hygiene_access_impediments.impediments_populationgroups_4",
                          "hygiene_access_impediments.impediments_populationgroups_5",
                          "nutrition_services.nutrition_services_1",
                          "nutrition_services.nutrition_services_2",
                          "nutrition_services.nutrition_services_3",
                          "nutrition_services.nutrition_services_4",
                          "nutrition_services.nutrition_services_5",
                          "nutrition_services.nutrition_services_6",
                          "nutrition_distributions.nutrition_distributions_1",
                          "nutrition_distributions.nutrition_distributions_2",
                          "nutrition_distributions.nutrition_distributions_3",
                          "nutrition_distributions.nutrition_distributions_4",
                          "nutrition_access_impediments.impediments_populationgroups_1",
                          "nutrition_access_impediments.impediments_populationgroups_2",
                          "nutrition_access_impediments.impediments_populationgroups_3",
                          "nutrition_access_impediments.impediments_populationgroups_4",
                          "nutrition_access_impediments.impediments_populationgroups_5",
                          "education_facilities.education_facilities_1",
                          "education_facilities.education_facilities_2",
                          "education_facilities.education_facilities_3",
                          "education_facilities.education_facilities_4",
                          "education_facilities_watersources",
                          "education_facilities_fence",
                          "education_access_impediments.impediments_populationgroups_1",
                          "education_access_impediments.impediments_populationgroups_2",
                          "education_access_impediments.impediments_populationgroups_3",
                          "education_access_impediments.impediments_populationgroups_4",
                          "education_access_impediments.impediments_populationgroups_5",
                          "foodsecurity_primary",
                          "foodsecurity_secondary",
                          "foodsecurity_livelihood",
                          "foodsecurity_land_livestock",
                          "foodsecurity_land_agriculture",
                          "foodsecurity_access",
                          "foodsecurity_access_impediments.impediments_populationgroups_1",
                          "foodsecurity_access_impediments.impediments_populationgroups_2",
                          "foodsecurity_access_impediments.impediments_populationgroups_3",
                          "foodsecurity_access_impediments.impediments_populationgroups_4",
                          "foodsecurity_access_impediments.impediments_populationgroups_5",
                          "protection_coveredspaces",	
                          "protection_womenspace",	
                          "protection_childfriendlyspace",
                          "protection_incidents.protection_incidents_1",
                          "protection_incidents.protection_incidents_2",
                          "protection_incidents.protection_incidents_3",
                          "protection_incidents.protection_incidents_4",
                          "protection_incidents.protection_incidents_5",
                          "protection_incidents.protection_incidents_6",
                          "protection_incidents.protection_incidents_7",
                          "protection_incidents.protection_incidents_8",
                          "protection_incidents.protection_incidents_9",
                          "protection_incidents.protection_incidents_10",
                          "protection_incidents.protection_incidents_11",
                          "protection_incidents.protection_incidents_12",
                          "protection_incidents.protection_incidents_13",
                          "protection_incidents.protection_incidents_14",
                          "protection_incidents.protection_incidents_15",
                          "protection_incidents_type.protection_incidents_type_1",
                          "protection_incidents_type.protection_incidents_type_2",
                          "protection_incidents_type.protection_incidents_type_3",
                          "protection_incidents_type.protection_incidents_type_4",
                          "protection_incidents_type.protection_incidents_type_5",
                          "protection_incidents_type.protection_incidents_type_6",
                          "protection_incidents_type.protection_incidents_type_7",
                          "protection_incidents_type.protection_incidents_type_8",
                          "protection_incidents_type.protection_incidents_type_9",
                          "protection_incidents_type.protection_incidents_type_10",
                          "protection_restrictions_day",
                          "protection_restrictions_night",
                          "support.support_1",
                          "support.support_2",
                          "support.support_3",
                          "support.support_4",
                          "support.support_5",
                          "support.support_6",
                          "support.support_7",
                          "support.support_8",
                          "support.support_9",
                          "support.support_10",
                          "support.support_11",
                          "support.support_12",
                          "support.support_13",
                          "support.support_14",
                          "support_access_impediments.impediments_populationgroups_1",
                          "support_access_impediments.impediments_populationgroups_2",
                          "support_access_impediments.impediments_populationgroups_3",
                          "support_access_impediments.impediments_populationgroups_4",
                          "support_access_impediments.impediments_populationgroups_5",
                          "aap_informationsources.aap_informationsources_1",
                          "aap_informationsources.aap_informationsources_2",
                          "aap_informationsources.aap_informationsources_3",
                          "aap_informationsources.aap_informationsources_4",
                          "aap_informationsources.aap_informationsources_5",
                          "aap_informationsources.aap_informationsources_6",
                          "aap_informationsources.aap_informationsources_7",
                          "aap_informationsources.aap_informationsources_8",
                          "aap_informationsources_pwd.aap_informationsources_pwd_1",
                          "aap_informationsources_pwd.aap_informationsources_pwd_2",
                          "aap_informationsources_pwd.aap_informationsources_pwd_3",
                          "aap_informationsources_pwd.aap_informationsources_pwd_4",
                          "aap_informationsources_pwd.aap_informationsources_pwd_5",
                          "aap_humanitarianassistanceproblems.aap_humanitarianassistanceproblems_1",
                          "aap_humanitarianassistanceproblems.aap_humanitarianassistanceproblems_2",
                          "aap_humanitarianassistanceproblems.aap_humanitarianassistanceproblems_3",
                          "aap_humanitarianassistanceproblems.aap_humanitarianassistanceproblems_4",
                          "aap_humanitarianassistanceproblems.aap_humanitarianassistanceproblems_5",
                          "aap_humanitarianassistanceproblems.aap_humanitarianassistanceproblems_6",
                          "aap_humanitarianassistanceproblems.aap_humanitarianassistanceproblems_7",
                          "aap_humanitarianassistanceproblems.aap_humanitarianassistanceproblems_8",
                          "aap_humanitarianassistanceproblems.aap_humanitarianassistanceproblems_9",
                          "aap_humanitarianassistanceproblems.aap_humanitarianassistanceproblems_10",
                          "aap_humanitarianassistanceproblems.aap_humanitarianassistanceproblems_11",
                          "aap_humanitarianassistanceproblems.aap_humanitarianassistanceproblems_12",
                          "aap_access_impediments.impediments_populationgroups_1",
                          "aap_access_impediments.impediments_populationgroups_2",
                          "aap_access_impediments.impediments_populationgroups_3",
                          "aap_access_impediments.impediments_populationgroups_4",
                          "aap_access_impediments.impediments_populationgroups_5",
                          "aap_languages.aap_languages_1",
                          "aap_languages.aap_languages_2",
                          "aap_languages.aap_languages_3",
                          "aap_languages.aap_languages_4",
                          "aap_languages.aap_languages_5",
                          "aap_languages.aap_languages_6",
                          "aap_languages.aap_languages_7",
                          "aap_languages.aap_languages_8",
                          "aap_languages.aap_languages_9",
                          "aap_languages.aap_languages_10")


for (i in list_response_checks){
  temp_name<-paste0(i)
  list_responses<-unique(data[[temp_name]])
  for (j in list_responses){ 
    temp_name_column_name<-paste("Question: ",i, " Response: ",j)
    dsa_hfc_enumerator_summary[, ncol(dsa_hfc_enumerator_summary) + 1] <- 100*(data %>%
                                                                                 group_by(deviceid, localisation_region) %>%
                                                                                 summarise(Val=sum(na.omit(get(temp_name)==j))) %>%
                                                                                 arrange(deviceid, localisation_region)) $Val/ (data %>%
                                                                                                                                  group_by(deviceid, localisation_region) %>%
                                                                                                                                  summarize(Nbr=length(na.omit(get(temp_name)))) %>%
                                                                                                                                  arrange(deviceid, localisation_region))$Nbr 
    names(dsa_hfc_enumerator_summary)[ncol(dsa_hfc_enumerator_summary)] <- paste0(temp_name_column_name)
    dsa_hfc_enumerator_summary[[temp_name_column_name]]<-paste(round(dsa_hfc_enumerator_summary[[temp_name_column_name]],digits=0),"%",sep="")
    }
}







dsa_hfc_enumerator_summary[[temp_name_column_name]] <-paste(round(dsa_hfc_enumerator_summary[[temp_name_column_name]],digits=0),"%",sep="")


data[[temp_name_column_name]] <- sum(na.omit(data[[temp_name]]==j))

data[[temp_name_response_sum]] <- as.integer(NA)
data[[temp_name_response_sum]]<- data[[temp_name_response_one]] + data[[temp_name_response_zero]]
data[[temp_name_response_percentage_one]] <- as.integer(NA)
data[[temp_name_response_percentage_one]]<- 100*(data[[temp_name_response_one]] / (data[[temp_name_response_one]] + data[[temp_name_response_zero]]))
data[[temp_name_response_percentage_zero]] <- as.integer(NA)
data[[temp_name_response_percentage_zero]]<- 100*(data[[temp_name_response_zero]] / (data[[temp_name_response_one]] + data[[temp_name_response_zero]]))
data[[temp_name_response_percentage_one]] <- paste(round(data[[temp_name_response_percentage_one]],digits=0),"%",sep="")
data[[temp_name_response_percentage_zero]] <- paste(round(data[[temp_name_response_percentage_zero]],digits=0),"%",sep="")

  
# Count: Number of Issues
# TO BE DETERMINED
#dsa_hfc_issues_log$Count<-as.integer(1)
#dsa_hfc_enumerator_summary[, ncol(dsa_hfc_enumerator_summary) + 1] <- (dsa_hfc_issues_log %>%
#                                                                         group_by(Enumerator, Region) %>%
#                                                                         summarise(Val=length(na.omit(Count))) %>%
#                                                                         arrange(Enumerator, Region)) $Val
#names(dsa_hfc_enumerator_summary)[ncol(dsa_hfc_enumerator_summary)] <- paste0("Count: Number of Issues")

# 3.08  Check summary statistics of key variables by enumerator
# TO BE DETERMINED

# Research Summary
# Check the frequencies of responses to key research variables.
# Check the frequencies of responses by treatment status.
# Check the frequencies of responses by demographic/geographic characteristics.
# Check for any variables with low response variance.
# Check refusal/not found rates by treatment status.

write.csv(dsa_hfc_issues_log, "outputs/dsa_issue_log.csv")
write.csv(dsa_hfc_survey_tracking, "outputs/dsa_survey_tracking.csv")
write.csv(dsa_hfc_enumerator_summary, "outputs/dsa_enumerator_summary.csv")


