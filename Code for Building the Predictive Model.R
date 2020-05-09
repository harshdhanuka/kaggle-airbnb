
# All data cleaning, transformation, feature engineering and the final model is in this file.

# Scroll to row 778 for final model.


# Load all the Libraries

rm(list=ls())

library(ISLR); library(ggplot2); library(caret); library(caTools); library(tidyr); library(dplyr); library(lm.beta);
library(leaps); library(car); library(mice); library(rapportools);
library(data.table); library(ngram); library(stringr); library(corrplot);
library(plyr); library(VIM); library(zipcode); library(zip); library(Metrics)
library(rpart); library(rpart.plot); library(randomForest); library(gbm); library(glmnet); library(xgboost)


# Set Working Directory

getwd(); setwd("/Users/harshdhanuka/Desktop/pricelala2")


# Read the 2 data sets, they have 3 types of NA's

data = read.csv('analysisData.csv', na.strings = c("NA","N/A",""))
scoringData = read.csv('scoringData.csv', na.strings = c("NA","N/A",""))


##########################################################################


# STEP 1: Dropping irrelevant variables from both data sets

data = subset(data, select = -c(id, host_name, host_location, host_acceptance_rate, host_neighbourhood, host_listings_count, street,
                                neighbourhood, city, state, market, country_code, country, square_feet, weekly_price, monthly_price,
                                has_availability, requires_license, license, jurisdiction_names, is_business_travel_ready))

scoringData = subset(scoringData, select = -c(host_name, host_location, host_acceptance_rate, host_neighbourhood, host_listings_count, street,
                                              neighbourhood, city, state, market, country_code, country, square_feet, weekly_price, monthly_price,
                                              has_availability, requires_license, license, jurisdiction_names, is_business_travel_ready))


########################################################################


# STEP 2: Check distribution of Price, Remove ASTRONOMICAL values

ggplot(data=data, aes(x=price)) + 
  geom_histogram(fill="blue", binwidth = 10)

table(data$price)       # 15 rows have Price = 0, which is not good data, need  to be removed


# Remove Rows which have Price = 0

data = data[!data$price==0,]
table(data$price)


########################################################################


# STEP 3: Using a corrplot to check for correlations between numeric variabels and also price


#first I isolate the numeric variables 
numericVars = which(sapply(data, is.numeric)) #index vector numeric variables

#then construct a correlation data frame
data_numVar = data[, numericVars]
cor_numVar = cor(data_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

#sorting on decreasing correlations with price
cor_sorted = as.matrix(sort(cor_numVar[,'price'], decreasing = TRUE))
print(cor_sorted)

#selecting only high corelations, greater than 0.1
CorHigh = names(which(apply(cor_sorted, 1, function(x) abs(x)>0.1)))
cor_numVar = cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

#These results will be impact my feature selction process moving forward


###########################################################


# STEP 4: Removing OUTLIERS from four columns


# MAXIMUM_NIGHTS
mask_maximum_nights_outlier = (data$maximum_nights>1200)
data[mask_maximum_nights_outlier, 'maximum_nights'] = median(data$maximum_nights,na.rm = T)
mask_maximum_nights_outlier1 = (scoringData$maximum_nights>1200)
scoringData[mask_maximum_nights_outlier1, 'maximum_nights'] = median(scoringData$maximum_nights,na.rm = T)
summary(data$maximum_nights)
plot(data$maximum_nights)

# MINIMUM_MAXIMUM_NIGHTS
mask_minimum_maximum_nights_outlier = (data$minimum_maximum_nights>1200)
data[mask_minimum_maximum_nights_outlier, 'minimum_maximum_nights'] = median(data$minimum_maximum_nights,na.rm = T)
mask_minimum_maximum_nights_outlier1 = (scoringData$maximum_nights>1200)
scoringData[mask_minimum_maximum_nights_outlier1, 'minimum_maximum_nights'] = median(scoringData$minimum_maximum_nights,na.rm = T)
summary(data$minimum_maximum_nights)
#plot(data$minimum_maximum_nights)

# MAXIMUM_MAXIMUM_NIGHTS
mask_maximum_maximum_nights_outlier = (data$maximum_maximum_nights>1200)
data[mask_maximum_maximum_nights_outlier, 'maximum_maximum_nights'] = median(data$maximum_maximum_nights,na.rm = T)
mask_maximum_maximum_nights_outlier1 = (scoringData$maximum_maximum_nights>1200)
scoringData[mask_maximum_maximum_nights_outlier1, 'maximum_maximum_nights'] = median(scoringData$maximum_maximum_nights,na.rm = T)
summary(data$maximum_maximum_nights)
#plot(data$maximum_maximum_nights)

# MAXIMUM_MAXIMUM_NIGHTS
mask_maximum_nights_avg_ntm_outlier = (data$maximum_nights_avg_ntm>1200)
data[mask_maximum_nights_avg_ntm_outlier, 'maximum_nights_avg_ntm'] = median(data$maximum_nights_avg_ntm,na.rm = T)
mask_maximum_nights_avg_ntm_outlier1 = (scoringData$maximum_nights_avg_ntm>1200)
scoringData[mask_maximum_nights_avg_ntm_outlier1, 'maximum_nights_avg_ntm'] = median(scoringData$maximum_nights_avg_ntm,na.rm = T)
summary(data$maximum_nights_avg_ntm)
#plot(data$maximum_nights_avg_ntm)


#########################################################################


# STEP 5: Find KEYWORD counts for luxury, in five text columns, FEATURE ENGINEERING


# Counts for luxury
data$luxury1 = data$name %like% "luxury"
scoringData$luxury1 = scoringData$name %like% "luxury"
data$luxury1 = as.numeric(as.logical(data$luxury1))
scoringData$luxury1 = as.numeric(as.logical(scoringData$luxury1))

data$luxury2 = data$summary %like% "luxury"
scoringData$luxury2 = scoringData$summary %like% "luxury"
data$luxury2 = as.numeric(as.logical(data$luxury2))
scoringData$luxury2 = as.numeric(as.logical(scoringData$luxury2))

data$luxury3 = data$space %like% "luxury"
scoringData$luxury3 = scoringData$space %like% "luxury"
data$luxury3 = as.numeric(as.logical(data$luxury3))
scoringData$luxury3 = as.numeric(as.logical(scoringData$luxury3))

data$luxury4 = data$description %like% "luxury"
scoringData$luxury4 = scoringData$description %like% "luxury"
data$luxury4 = as.numeric(as.logical(data$luxury4))
scoringData$luxury4 = as.numeric(as.logical(scoringData$luxury4))

data$luxury5 = data$neighborhood_overview %like% "luxury"
scoringData$luxury5 = scoringData$neighborhood_overview %like% "luxury"
data$luxury5 = as.numeric(as.logical(data$luxury5))
scoringData$luxury5 = as.numeric(as.logical(scoringData$luxury5))


# Counts for Luxury
data$luxury6 = data$name %like% "Luxury"
scoringData$luxury6 = scoringData$name %like% "Luxury"
data$luxury6 = as.numeric(as.logical(data$luxury6))
scoringData$luxury6 = as.numeric(as.logical(scoringData$luxury6))

data$luxury7 = data$summary %like% "Luxury"
scoringData$luxury7 = scoringData$summary %like% "Luxury"
data$luxury7 = as.numeric(as.logical(data$luxury7))
scoringData$luxury7 = as.numeric(as.logical(scoringData$luxury7))

data$luxury8 = data$space %like% "Luxury"
scoringData$luxury8 = scoringData$space %like% "Luxury"
data$luxury8 = as.numeric(as.logical(data$luxury8))
scoringData$luxury8 = as.numeric(as.logical(scoringData$luxury8))

data$luxury9 = data$description %like% "Luxury"
scoringData$luxury9 = scoringData$description %like% "Luxury"
data$luxury9 = as.numeric(as.logical(data$luxury9))
scoringData$luxury9 = as.numeric(as.logical(scoringData$luxury9))

data$luxury10 = data$neighborhood_overview %like% "Luxury"
scoringData$luxury10 = scoringData$neighborhood_overview %like% "Luxury"
data$luxury10 = as.numeric(as.logical(data$luxury10))
scoringData$luxury10 = as.numeric(as.logical(scoringData$luxury10))


# Counts for luxurious
data$luxury11 = data$name %like% "luxurious"
scoringData$luxury11 = scoringData$name %like% "luxurious"
data$luxury11 = as.numeric(as.logical(data$luxury11))
scoringData$luxury11 = as.numeric(as.logical(scoringData$luxury11))

data$luxury12 = data$summary %like% "luxurious"
scoringData$luxury12 = scoringData$summary %like% "luxurious"
data$luxury12 = as.numeric(as.logical(data$luxury12))
scoringData$luxury12 = as.numeric(as.logical(scoringData$luxury12))

data$luxury13 = data$space %like% "luxurious"
scoringData$luxury13 = scoringData$space %like% "luxurious"
data$luxury13 = as.numeric(as.logical(data$luxury13))
scoringData$luxury13 = as.numeric(as.logical(scoringData$luxury13))

data$luxury14 = data$description %like% "luxurious"
scoringData$luxury14 = scoringData$description %like% "luxurious"
data$luxury14 = as.numeric(as.logical(data$luxury14))
scoringData$luxury14 = as.numeric(as.logical(scoringData$luxury14))

data$luxury15 = data$neighborhood_overview %like% "luxurious"
scoringData$luxury15 = scoringData$neighborhood_overview %like% "luxurious"
data$luxury15 = as.numeric(as.logical(data$luxury15))
scoringData$luxury15 = as.numeric(as.logical(scoringData$luxury15))


# Counts for Luxurioius
data$luxury16 = data$name %like% "Luxurious"
scoringData$luxury16 = scoringData$name %like% "Luxurious"
data$luxury16 = as.numeric(as.logical(data$luxury16))
scoringData$luxury16 = as.numeric(as.logical(scoringData$luxury16))

data$luxury17 = data$summary %like% "Luxurious"
scoringData$luxury17 = scoringData$summary %like% "Luxurious"
data$luxury17 = as.numeric(as.logical(data$luxury17))
scoringData$luxury17 = as.numeric(as.logical(scoringData$luxury17))

data$luxury18 = data$space %like% "Luxurious"
scoringData$luxury18 = scoringData$space %like% "Luxurious"
data$luxury18 = as.numeric(as.logical(data$luxury18))
scoringData$luxury18 = as.numeric(as.logical(scoringData$luxury18))

data$luxury19 = data$description %like% "Luxurious"
scoringData$luxury19 = scoringData$description %like% "Luxurious"
data$luxury19 = as.numeric(as.logical(data$luxury19))
scoringData$luxury19 = as.numeric(as.logical(scoringData$luxury19))

data$luxury20 = data$neighborhood_overview %like% "Luxurious"
scoringData$luxury20 = scoringData$neighborhood_overview %like% "Luxurious"
data$luxury20 = as.numeric(as.logical(data$luxury120))
scoringData$luxury20 = as.numeric(as.logical(scoringData$luxury20))


# Counting the SUM for all columns, only the new column created here will be used further

data$luxury = (data$luxury1 + data$luxury2 + data$luxury3 + data$luxury4 + data$luxury5 + data$luxury6 + data$luxury7 + data$luxury8 + data$luxury9 + data$luxury10
               + data$luxury11 + data$luxury12 + data$luxury13 + data$luxury14 + data$luxury15 + data$luxury16 + data$luxury17 + data$luxury18 + data$luxury19 
               + data$luxury20)

scoringData$luxury = (scoringData$luxury1 + scoringData$luxury2 + scoringData$luxury3 + scoringData$luxury4 + scoringData$luxury5 + scoringData$luxury6
                      + scoringData$luxury7 + scoringData$luxury8 + scoringData$luxury9 + scoringData$luxury10 + scoringData$luxury11 + scoringData$luxury12
                      + scoringData$luxury13 + scoringData$luxury14 + scoringData$luxury15 + scoringData$luxury16 + scoringData$luxury17 + scoringData$luxury18
                      + scoringData$luxury19 + scoringData$luxury20)


## OR, Alternative Approach

# Concatenate all text columns
text_columns = c("name", "summary", "space", "description", "neighborhood_overview", "notes", "transit", "access", "interaction", "house_rules", "host_about")
data$concatenate_texts = do.call(paste, c(data[text_columns], sep = ""))
scoringData$concatenate_texts = do.call(paste, c(scoringData[text_columns], sep = ""))

# Count the Keyword 'luxur' ignoring case, which includes all four variations as above 
data$luxury = as.factor(grepl('luxur',data$concatenate_texts,ignore.case=T))
scoringData$luxury = as.factor(grepl('luxur',scoringData$concatenate_texts,ignore.case=T))


###################################################


# STEP 6:  NAME column


# Number of UPPERCASE letters in names, FEATURE ENGINEERING
data$name_upper = str_count(data$name, "[A-Z]")
scoringData$name_upper = str_count(scoringData$name, "[A-Z]")


#  Convert NAME to Word Counts
data$name = str_count(data$name, pattern = " ")
scoringData$name = str_count(scoringData$name,pattern = " ")


####################################################


# STEP 7:  TEXT Columns - Do WORD COUNT, and Remove NA's


# SUMMARY
data$summary = str_count(data$summary, pattern = " ")
scoringData$summary = str_count(scoringData$summary,pattern = " ")
mask_summary_NAs = is.na(data$summary)
data[mask_summary_NAs, 'summary'] = median(data$summary,na.rm = T)
mask_summary_NAs1 = is.na(scoringData$summary)
scoringData[mask_summary_NAs1, 'summary'] = median(scoringData$summary,na.rm = T)

# SPACE
data$space = str_count(data$space, pattern = " ")
scoringData$space = str_count(scoringData$space,pattern = " ")
mask_space_NAs = is.na(data$space)
data[mask_space_NAs, 'space'] = median(data$space, na.rm = T)
mask_space_NAs1 = is.na(scoringData$space)
scoringData[mask_space_NAs1, 'space'] = median(scoringData$space, na.rm = T)

# DESCRIPTION
data$description = str_count(data$description, pattern = " ")
scoringData$description = str_count(scoringData$description,pattern = " ")
mask_description_NAs = is.na(data$description)
data[mask_description_NAs, 'description'] = median(data$description, na.rm = T)
mask_description_NAs1 = is.na(scoringData$description)
scoringData[mask_description_NAs1, 'description'] = median(scoringData$description, na.rm = T)

# NEIGHBORHOOD_OVERVIEW
data$neighborhood_overview = str_count(data$neighborhood_overview, pattern = " ")
scoringData$neighborhood_overview = str_count(scoringData$neighborhood_overview,pattern = " ")
mask_neighborhood_overview_NAs = is.na(data$neighborhood_overview)
data[mask_neighborhood_overview_NAs, 'neighborhood_overview'] = median(data$neighborhood_overview, na.rm = T)
mask_neighborhood_overview_NAs1 = is.na(scoringData$neighborhood_overview)
scoringData[mask_neighborhood_overview_NAs1, 'neighborhood_overview'] = median(scoringData$neighborhood_overview, na.rm = T)

# NOTES
data$notes = str_count(data$notes, pattern = " ")
scoringData$notes = str_count(scoringData$notes,pattern = " ")
mask_notes_NAs = is.na(data$notes)
data[mask_notes_NAs, 'notes'] = median(data$notes, na.rm = T)
mask_notes_NAs1 = is.na(scoringData$notes)
scoringData[mask_notes_NAs1, 'notes'] = median(scoringData$notes, na.rm = T)

# TRANSIT
data$transit = str_count(data$transit, pattern = " ")
scoringData$transit = str_count(scoringData$transit,pattern = " ")
mask_transit_NAs = is.na(data$transit)
data[mask_transit_NAs, 'transit'] = median(data$transit, na.rm = T)
mask_transit_NAs1 = is.na(scoringData$transit)
scoringData[mask_transit_NAs1, 'transit'] = median(scoringData$transit, na.rm = T)

# ACCESS
data$access = str_count(data$access, pattern = " ")
scoringData$access = str_count(scoringData$access,pattern = " ")
mask_access_NAs = is.na(data$access)
data[mask_access_NAs, 'access'] = median(data$access, na.rm = T)
mask_access_NAs1 = is.na(scoringData$access)
scoringData[mask_access_NAs1, 'access'] = median(scoringData$access, na.rm = T)

# INTERACTION
data$interaction = str_count(data$interaction, pattern = " ")
scoringData$interaction = str_count(scoringData$interaction,pattern = " ")
mask_interaction_NAs = is.na(data$interaction)
data[mask_interaction_NAs, 'interaction'] = median(data$interaction, na.rm = T)
mask_interaction_NAs1 = is.na(scoringData$interaction)
scoringData[mask_interaction_NAs1, 'interaction'] = median(scoringData$interaction, na.rm = T)

# HOST_ABOUT
data$host_about = str_count(data$host_about, pattern = " ")
scoringData$host_about = str_count(scoringData$host_about,pattern = " ")
mask_host_about_NAs = is.na(data$host_about)
data[mask_host_about_NAs, 'host_about'] = median(data$host_about, na.rm = T)
mask_host_about_NAs1 = is.na(scoringData$host_about)
scoringData[mask_host_about_NAs1, 'host_about'] = median(scoringData$host_about, na.rm = T)

# HOUSE_RULES
data$house_rules = str_count(data$house_rules, pattern = " ")
scoringData$house_rules = str_count(scoringData$house_rules,pattern = " ")
mask_house_rules_NAs = is.na(data$house_rules)
data[mask_house_rules_NAs, 'house_rules'] = median(data$house_rules, na.rm = T)
mask_house_rules_NAs1 = is.na(scoringData$house_rules)
scoringData[mask_house_rules_NAs1, 'house_rules'] = median(scoringData$house_rules, na.rm = T)


########################################################################


# STEP 8: LOGICAL columns - Remove NA's and revalue to logical format


# HOST_IS_SUPERHOST
mask_host_is_superhost_NAs = is.na(data$host_is_superhost)
data[mask_host_is_superhost_NAs, 'host_is_superhost'] = "f"
mask_host_is_superhost_NAs1 = is.na(scoringData$host_is_superhost)
scoringData[mask_host_is_superhost_NAs1, 'host_is_superhost'] = "f"
data$host_is_superhost = revalue(data$host_is_superhost, c("f"="FALSE", "t"="TRUE"))
data$host_is_superhost = as.logical(data$host_is_superhost)
scoringData$host_is_superhost = revalue(scoringData$host_is_superhost, c("f"="FALSE", "t"="TRUE"))
scoringData$host_is_superhost = as.logical(scoringData$host_is_superhost)

# HOST_HAS_PROFILE_PIC
mask_host_has_profile_pic_NAs = is.na(data$host_has_profile_pic)
data[mask_host_has_profile_pic_NAs, 'host_has_profile_pic'] = "f"
mask_host_has_profile_pic_NAs1 = is.na(scoringData$host_has_profile_pic)
scoringData[mask_host_has_profile_pic_NAs1, 'host_has_profile_pic'] = "f"
data$host_has_profile_pic = revalue(data$host_has_profile_pic, c("f"="FALSE", "t"="TRUE"))
data$host_has_profile_pic = as.logical(data$host_has_profile_pic)
scoringData$host_has_profile_pic = revalue(scoringData$host_has_profile_pic, c("f"="FALSE", "t"="TRUE"))
scoringData$host_has_profile_pic = as.logical(scoringData$host_has_profile_pic)

# HOST_IDENTITY_VERIFIED
mask_host_identity_verified_NAs = is.na(data$host_identity_verified)
data[mask_host_identity_verified_NAs, 'host_identity_verified'] = "f"
mask_host_identity_verified_NAs1 = is.na(scoringData$host_identity_verified)
scoringData[mask_host_identity_verified_NAs1, 'host_identity_verified'] = "f"
data$host_identity_verified = revalue(data$host_identity_verified, c("f"="FALSE", "t"="TRUE"))
data$host_identity_verified = as.logical(data$host_identity_verified)
scoringData$host_identity_verified = revalue(scoringData$host_identity_verified, c("f"="FALSE", "t"="TRUE"))
scoringData$host_identity_verified = as.logical(scoringData$host_identity_verified)


######################################################################


# STEP 9:  Remove NA's from Other CATEGORICAL and NUMERIC Columns


# HOST_RESPONSE_TIME
mask_host_response_time_NAs = is.na(data$host_response_time)
data[mask_host_response_time_NAs, 'host_response_time'] = "a few days or more"
mask_host_response_time_NAs1 = is.na(scoringData$host_response_time)
scoringData[mask_host_response_time_NAs1, 'host_response_time'] = "a few days or more"


# HOST_RESPONSE_RATE
# Convert factor to character
data$host_response_rate = as.character(data$host_response_rate)
scoringData$host_response_rate = as.character(scoringData$host_response_rate)
# Remove the percentage % sign, then convert to integer, and fill NA's
data$host_response_rate = as.integer(substr(data$host_response_rate, 1, nchar(data$host_response_rate)-1))
scoringData$host_response_rate = as.integer(substr(scoringData$host_response_rate, 1, nchar(scoringData$host_response_rate)-1))
mask_host_response_rate_NAs = is.na(data$host_response_rate)
data[mask_host_response_rate_NAs, 'host_response_rate'] = median(data$host_response_rate,na.rm = T)
mask_host_response_rate_NAs1 = is.na(scoringData$host_response_rate)
scoringData[mask_host_response_rate_NAs1, 'host_response_rate'] = median(scoringData$host_response_rate,na.rm = T)


# HOST_TOTAL_LISTINGS_COUNT
mask_host_total_listings_count_NAs = is.na(data$host_total_listings_count)
data[mask_host_total_listings_count_NAs, 'host_total_listings_count'] = median(data$host_total_listings_count,na.rm = T)
mask_host_total_listings_count_NAs1 = is.na(scoringData$host_total_listings_count)
scoringData[mask_host_total_listings_count_NAs1, 'host_total_listings_count'] = median(scoringData$host_total_listings_count,na.rm = T)

# BEDS
mask_beds_NAs = is.na(data$beds)
data[mask_beds_NAs, 'beds'] = median(data$beds,na.rm = T)
mask_beds_NAs1 = is.na(scoringData$beds)
scoringData[mask_beds_NAs1, 'beds'] = median(scoringData$beds,na.rm = T)

# SECURITY_DEPOSIT
mask_security_deposit_NAs = is.na(data$security_deposit)
data[mask_security_deposit_NAs, 'security_deposit'] = median(data$security_deposit,na.rm = T)
mask_security_deposit_NAs1 = is.na(scoringData$security_deposit)
scoringData[mask_security_deposit_NAs1, 'security_deposit'] = median(scoringData$security_deposit,na.rm = T)

# CLEANING_FEE
mask_cleaning_fee_NAs = is.na(data$cleaning_fee)
data[mask_cleaning_fee_NAs, 'cleaning_fee'] = median(data$cleaning_fee,na.rm = T)
mask_cleaning_fee_NAs1 = is.na(scoringData$cleaning_fee)
scoringData[mask_cleaning_fee_NAs1, 'cleaning_fee'] = median(scoringData$cleaning_fee,na.rm = T)

# REVIEWS_PER_MONTH
mask_reviews_per_month_NAs = is.na(data$reviews_per_month)
data[mask_reviews_per_month_NAs, 'reviews_per_month'] = median(data$reviews_per_month,na.rm = T)
mask_reviews_per_month_NAs1 = is.na(scoringData$reviews_per_month)
scoringData[mask_reviews_per_month_NAs1, 'reviews_per_month'] = median(scoringData$reviews_per_month,na.rm = T)


##################################################################


# STEP 10:  Other FEATURE ENGINEERING  -  Count Real Beds, Convert to LIST format and Amenities LOGICALS


# HOST_VERIFICATIONS
# count the number of commas, and add + 1, beacuse for the last value there is no comma to count
data$host_verifications = (str_count(data$host_verifications, pattern = ',')) + 1
scoringData$host_verifications = (str_count(scoringData$host_verifications, pattern = ',')) + 1


# BED_TYPE
# Real Bed - Yes / No?
data$real_bed = data$bed_type %like% "Real Bed"
scoringData$real_bed = scoringData$bed_type %like% "Real Bed"
data$real_bed = as.integer(data$real_bed)
scoringData$real_bed = as.integer(scoringData$real_bed)


# AMENITIES
data$amenities_list = (str_count(data$amenities, pattern = ',')) + 1
scoringData$amenities_list = (str_count(scoringData$amenities, pattern = ',')) + 1


# SPLIT AMENITIES - WIFI, TV, AIR CONDITIONING, HEATING, ELEVATOR, KITCHEN, ESSENTIALS, WASHER, DRYER,
#                   Parking Premises, Parking Street, Gym, Internet  -  to LOGICAL

# WIFI
data$amenities_Wifi = data$amenities %like% "Wifi"
scoringData$amenities_Wifi = scoringData$amenities %like% "Wifi"
data$amenities_Wifi = as.logical(data$amenities_Wifi)
scoringData$amenities_Wifi = as.logical(scoringData$amenities_Wifi)

# TV
data$amenities_TV = data$amenities %like% "TV"
scoringData$amenities_TV = scoringData$amenities %like% "TV"
data$amenities_TV = as.logical(data$amenities_TV)
scoringData$amenities_TV = as.logical(scoringData$amenities_TV)

# AIR CONDITIONING
data$amenities_Air_conditioning = data$amenities %like% "Air conditioning"
scoringData$amenities_Air_conditioning = scoringData$amenities %like% "Air conditioning"
data$amenities_Air_conditioning = as.logical(data$amenities_Air_conditioning)
scoringData$amenities_Air_conditioning = as.logical(scoringData$amenities_Air_conditioning)

# HEATING
data$amenities_Heating = data$amenities %like% "Heating"
scoringData$amenities_Heating = scoringData$amenities %like% "Heating"
data$amenities_Heating = as.logical(data$amenities_Heating)
scoringData$amenities_Heating = as.logical(scoringData$amenities_Heating)

# ELEVATOR
data$amenities_Elevator = data$amenities %like% "Elevator"
scoringData$amenities_Elevator = scoringData$amenities %like% "Elevator"
data$amenities_Elevator = as.logical(data$amenities_Elevator)
scoringData$amenities_Elevator = as.logical(scoringData$amenities_Elevator)

# KITCHEN
data$amenities_Kitchen = data$amenities %like% "Kitchen"
scoringData$amenities_Kitchen = scoringData$amenities %like% "Kitchen"
data$amenities_Kitchen = as.logical(data$amenities_Kitchen)
scoringData$amenities_Kitchen = as.logical(scoringData$amenities_Kitchen)

# ESSENTIALS
data$amenities_Essentials = data$amenities %like% "Essentials"
scoringData$amenities_Essentials = scoringData$amenities %like% "Essentials"
data$amenities_Essentials = as.logical(data$amenities_Essentials)
scoringData$amenities_Essentials = as.logical(scoringData$amenities_Essentials)

# WASHER
data$amenities_Washer = data$amenities %like% "Washer"
scoringData$amenities_Washer = scoringData$amenities %like% "Washer"
data$amenities_Washer = as.logical(data$amenities_Washer)
scoringData$amenities_Washer = as.logical(scoringData$amenities_Washer)

# DRYER
data$amenities_Dryer = data$amenities %like% "Dryer"
scoringData$amenities_Dryer = scoringData$amenities %like% "Dryer"
data$amenities_Dryer = as.logical(data$amenities_Dryer)
scoringData$amenities_Dryer = as.logical(scoringData$amenities_Dryer)

# FREE PARKING ON PREMISES
data$amenities_parking_premises = data$amenities %like% "Free parking on premises"
scoringData$amenities_parking_premises = scoringData$amenities %like% "Free parking on premises"
data$amenities_parking_premises = as.logical(data$amenities_parking_premises)
scoringData$amenities_parking_premises = as.logical(scoringData$amenities_parking_premises)

# FREE STREET PARKING
data$amenities_parking_street = data$amenities %like% "Free street parking"
scoringData$amenities_parking_street = scoringData$amenities %like% "Free street parking"
data$amenities_parking_street = as.logical(data$amenities_parking_street)
scoringData$amenities_parking_street = as.logical(scoringData$amenities_parking_street)

# GYM
data$amenities_gym = data$amenities %like% "Gym"
scoringData$amenities_gym = scoringData$amenities %like% "Gym"
data$amenities_gym = as.logical(data$amenities_gym)
scoringData$amenities_gym = as.logical(scoringData$amenities_gym)

# INTERNET
data$amenities_internet = data$amenities %like% "Internet"
scoringData$amenities_internet = scoringData$amenities %like% "Internet"
data$amenities_internet = as.logical(data$amenities_internet)
scoringData$amenities_internet = as.logical(scoringData$amenities_internet)


##########################################################################


# STEP 11:  Other FEATURE ENGINEERING - Create Average Minimum Nights and Average Maximum Nights


# MINIMUM_MINIMUM_NIGHTS and MAXIMUM_MINIMUM_NIGHTS  -  AVERAGE_MINIMUM_NIGHTS
data$average_minimum_nights = (data$maximum_minimum_nights + data$minimum_minimum_nights)/2
scoringData$average_minimum_nights = (scoringData$maximum_minimum_nights + scoringData$minimum_minimum_nights)/2

# MINIMUM_MAXIMUM_NIGHTS and MAXIMUM_MAXIMUM_NIGHTS  -  AVERAGE_MAXIMUM_NIGHTS
data$average_maximum_nights = (data$maximum_maximum_nights + data$minimum_maximum_nights)/2
scoringData$average_maximum_nights = (scoringData$maximum_maximum_nights + scoringData$minimum_maximum_nights)/2


######################################################################


# STEP 12:  DATE  - Convert to Date Format, Count Number of DAYS till today, Replace NA's - FEATURE ENGINEERING


# HOST_SINCE
# Convert to date format
data$host_since = as.Date(data$host_since)
scoringData$host_since = as.Date(scoringData$host_since)
# Replace NA's with median date
mask_host_since_NAs = is.na(data$host_since)
data[mask_host_since_NAs, 'host_since'] = median(data$host_since,na.rm = T)
mask_host_since_NAs1 = is.na(scoringData$host_since)
scoringData[mask_host_since_NAs1, 'host_since'] = median(scoringData$host_since,na.rm = T)
# now, count number of days till today from given date value
data$host_since_days = as.integer(Sys.Date() - data$host_since)
scoringData$host_since_days = as.integer(Sys.Date() - scoringData$host_since)

# FIRST_REVIEW
# Convert to date format
data$first_review = as.Date(data$first_review)
scoringData$first_review = as.Date(scoringData$first_review)
# Replace NA's with median date
mask_first_review_NAs = is.na(data$first_review)
data[mask_first_review_NAs, 'first_review'] = median(data$first_review,na.rm = T)
mask_first_review_NAs1 = is.na(scoringData$first_review)
scoringData[mask_first_review_NAs1, 'first_review'] = median(scoringData$first_review,na.rm = T)
# now, count number of days till today from given date value
data$first_review_days = as.integer(Sys.Date() - data$first_review)
scoringData$first_review_days = as.integer(Sys.Date() - scoringData$first_review)

# LAST_REVIEW
# Convert to date format
data$last_review = as.Date(data$last_review)
scoringData$last_review = as.Date(scoringData$last_review)
# Replace NA's with median date
mask_last_review_NAs = is.na(data$last_review)
data[mask_last_review_NAs, 'last_review'] = median(data$last_review,na.rm = T)
mask_last_review_NAs1 = is.na(scoringData$last_review)
scoringData[mask_last_review_NAs1, 'last_review'] = median(scoringData$last_review,na.rm = T)
# now, count number of days till today from given date value
data$last_review_days = as.integer(Sys.Date() - data$last_review)
scoringData$last_review_days = as.integer(Sys.Date() - scoringData$last_review)


######################################################################


# STEP 13:  DUMMY ENCODE, Revalue Logicals


# NEIGHBOURHOOD_CLEANSED
#data$neighbourhood_cleansed = as.integer(data$neighbourhood_cleansed)
#scoringData$neighbourhood_cleansed = as.integer(scoringData$neighbourhood_cleansed)

# NEIGHBOURHOOD_GROUP_CLEANSED
#data$neighbourhood_group_cleansed = as.integer(data$neighbourhood_group_cleansed)
#scoringData$neighbourhood_group_cleansed = as.integer(scoringData$neighbourhood_group_cleansed)

# SMART_LOCATION
#data$smart_location = as.integer(data$smart_location)
#scoringData$smart_location = as.integer(scoringData$smart_location)

# IS_LOCATION_EXACT
data$is_location_exact = revalue(data$is_location_exact, c("f"="FALSE", "t"="TRUE"))
data$is_location_exact = as.logical(data$is_location_exact)
scoringData$is_location_exact = revalue(scoringData$is_location_exact, c("f"="FALSE", "t"="TRUE"))
scoringData$is_location_exact = as.logical(scoringData$is_location_exact)

# ROOM_TYPE
#data$room_type = as.integer(data$room_type)
#coringData$room_type = as.integer(scoringData$room_type)

# BED_TYPE
# data$bed_type = as.integer(data$bed_type)
# scoringData$bed_type = as.integer(scoringData$bed_type)

# CALENDAR_UPDATED
#data$calendar_updated = as.integer(data$calendar_updated)
#scoringData$calendar_updated = as.integer(scoringData$calendar_updated)

# INSTANT_BOOKABLE
data$instant_bookable = revalue(data$instant_bookable, c("f"="FALSE", "t"="TRUE"))
data$instant_bookable = as.logical(data$instant_bookable)
scoringData$instant_bookable = revalue(scoringData$instant_bookable, c("f"="FALSE", "t"="TRUE"))
scoringData$instant_bookable = as.logical(scoringData$instant_bookable)

# CANCELLATION_POLICY
#data$cancellation_policy = as.integer(data$cancellation_policy)
#scoringData$cancellation_policy = as.integer(scoringData$cancellation_policy)

# REQUIRE_GUEST_PROFILE_PICTURE
data$require_guest_profile_picture = revalue(data$require_guest_profile_picture, c("f"="FALSE", "t"="TRUE"))
data$require_guest_profile_picture = as.logical(data$require_guest_profile_picture)
scoringData$require_guest_profile_picture = revalue(scoringData$require_guest_profile_picture, c("f"="FALSE", "t"="TRUE"))
scoringData$require_guest_profile_picture = as.logical(scoringData$require_guest_profile_picture)

# REQUIRE_GUEST_PHONE_VERIFICATION
data$require_guest_phone_verification = revalue(data$require_guest_phone_verification, c("f"="FALSE", "t"="TRUE"))
data$require_guest_phone_verification = as.logical(data$require_guest_phone_verification)
scoringData$require_guest_phone_verification = revalue(scoringData$require_guest_phone_verification, c("f"="FALSE", "t"="TRUE"))
scoringData$require_guest_phone_verification = as.logical(scoringData$require_guest_phone_verification)


#####################################################################


# STEP 14:  Other FEATURE ENGINEERING - PROPERTY_TYPE groups - did not have significant effect

# PROPERTY_TYPE

#a="Apartment"
#b="Hotel"
#c="House"
#d="Room/Cabin/BnB"
#e="Resort"
#f="Camper/RV/Boat"
#g="Others"

#levels(data[,'property_type'])=c(b,a,d,f,b,c,d,f,g,c,e,c,c,a,c,d,b,c,f,g,d,e,g,g,e,a,f,g,g,d,e)
#levels(data[,'property_type'])
#levels(scoringData[,'property_type'])=c(b,a,d,f,b,c,f,c,c,c,e,c,e,a,c,d,b,c,d,g,e,a,g,d,e)
#levels(scoringData[,'property_type'])

#data$property_type = as.integer(data$property_type)
#scoringData$property_type = as.integer(scoringData$property_type)


#####################################################################


# STEP 15:  Cleaning ZIPCODE, Removing NA's


# ZIPCODE
# Replace NA's with mode of zipcode
mask_zipcode_NAs = is.na(data$zipcode)
data[mask_zipcode_NAs, 'zipcode'] = "11221"
mask_zipcode_NAs1 = is.na(scoringData$zipcode)
scoringData[mask_zipcode_NAs1, 'zipcode'] = "11211"

# Clean mis-represented zipcodes
data$zipcode[data$zipcode == "11249\n11249"] = "11249"
data$zipcode[data$zipcode == "11385-2308"] = "11385"
data$zipcode[data$zipcode == "11413-3220"] = "11413"
data$zipcode[data$zipcode == "11103-3233"] = "11103"
data$zipcode[data$zipcode == "1m"] = "11221"

scoringData$zipcode[scoringData$zipcode == "11103-3233"] = "11103"
scoringData$zipcode[scoringData$zipcode == "111211"] = "11221"


##############################################################################


# STEP 16:  DROP all other non-relevant columns


data1 = subset(data, select = -c(host_since, amenities,first_review,last_review,
                                 luxury1,luxury2,luxury3,luxury4,luxury5,luxury6,luxury7,luxury8,luxury9,
                                 luxury10,luxury11,luxury12,luxury13,luxury14,luxury15,luxury16,luxury17,
                                 luxury18,luxury19,luxury20))

scoringData1 = subset(scoringData, select = -c(host_since, amenities,first_review,last_review,
                                               luxury1,luxury2,luxury3,luxury4,luxury5,
                                               luxury6,luxury7,luxury8,luxury9,luxury10,
                                               luxury11,luxury12,luxury13,luxury14,luxury15,
                                               luxury16,luxury17,luxury18,luxury19,luxury20))                   


# Create new Analysis Data file so dont have to run above steps again
write.csv(data1, "AnalysisClean.csv",row.names = F)
write.csv(scoringData1, "ScoringClean.csv",row.names = F)


###############################################################################################


# IMPUTING WITH MICE  instead of Median for all NA's  -  did not have significant effect, so went ahead with median imputation


# imputedata = mice(data1[,c(2:15,17,18,28,30,31,61,80,81,82)], m=4, method = "cart", seed = 123, printFlag = FALSE)
# imputescoringData = mice(scoringData1[,c(2:9,12:16,18,19,29,32,33,84)], m=4, method = "cart", seed = 123, printFlag = FALSE)

#newimpdata = mice::complete(imputedata)
#newimpscoringData = mice::complete(imputescoringData)

# Drop Old Variable Colums which had NA
#data1 = subset(data1, select = -c(2:15,17,18,28,30,31,61,80,81,82))
#scoringData1 = subset(scoringData1, select = -c(2:9,12:16,18,19,29,32,33,84))

# Bind new Impute colums  to Original data sets
#data2 = cbind(data1,newimpdata)
#scoringData2 = cbind(id = scoringData$id,scoringData1,newimpscoringData)


################################################################################################


# FINAL STEP  -  Build XGBOOST Model


# Read cleaned data
data = read.csv('AnalysisClean.csv')
scoringData = read.csv('ScoringCean.csv')


# Train / Test Split
set.seed(123)
split = createDataPartition(
  y = data$price,
  p = 0.8,
  list = F,
  groups = 100)

traint = data[split, ]
testt = data[-split, ]

nrow(traint) + nrow(testt) == nrow(data)
nrow(traint)
nrow(testt)
nrow(data)


# Build XGBOOST Model on Train Set

set.seed(123)

train = traint %>%
  na.omit() %>%
  mutate_if(is.character, as.factor) 

test = testt %>%
  na.omit() %>%
  mutate_if(is.character, as.factor) 

# Remove the price column from the train and test
train1 = train %>% select(-price)
test1 = test %>% select(-price)

# Convert to data matrix for use in XGBOOST, set label
trainA = data.matrix(train1)
trainA_label = data.matrix(train$price)

testA = data.matrix(test1)
testA_label = data.matrix(test$price)


# Build Model
bst = xgboost(data = trainA, 
               label = trainA_label,
               eta = 0.04,
               nrounds = 700,
               max_depth = 6,
               min_child_weight = 15,
               subsample = 0.8,
               objective = "reg:linear",
               eval_metric = "rmse",
               verbose = 0)

# Predict and Check RMSE on TEST
predA = predict(bst, newdata = testA)  
rmse(testA_label, predA)

# Predict and Check RMSE on TRAIN
predA = predict(bst, newdata = trainA)  
rmse(trainA_label, predA)


##############################


# USE ENTIRE DATA

set.seed(123)

train = data %>%
  na.omit() %>%
  mutate_if(is.character, as.factor) 

test = scoringData %>%
  na.omit() %>%
  mutate_if(is.character, as.factor) 

# Remove the price colum from the train, and id column from test
train1 = train %>% select(-price)
test1 = test %>% select(-id)

# Convert to data matrix for use in XGBOOST, set label
trainA = data.matrix(train1)
trainA_label = data.matrix(train$price)

testA = data.matrix(test1)


# Build Model
bst = xgboost(data = trainA, 
               label = trainA_label,
               eta = 0.04,
               nrounds = 700,
               max_depth = 6,
               min_child_weight = 15,
               subsample = 0.8,
               objective = "reg:linear",
               eval_metric = "rmse",
               verbose = 0)

# Predict and Check RMSE on TRAIN
predA = predict(bst, newdata = trainA)  
rmse(trainA_label, predA)


# Predict on the TEST
predA = predict(bst, newdata = testA)  

# Create Submission File
submissionFile = data.frame(id = scoringData$id, price = predA)
write.csv(submissionFile, 'Submission.csv',row.names = F)


# NOTE :   My best submission was a few days ago, I do not remember the exact feature engineering and the exact parameters used for that.
#          This is still very close to the original best submission. RMSE difference would only be very minimal.


################################################   THE END   #####################################################
