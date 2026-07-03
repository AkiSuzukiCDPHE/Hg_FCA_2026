# SITE SPECIFIC ADVISORIES FOR MERCURY FOR GP

library(dplyr)

# Create a new data frame using the final data frame from the regression script 
Hg_SS <- HgRegression_Final

# Analysis 1 - Censoring the data for the ideal averaging period. 
# For mercury always censor the data to be within the last 10 years.
# Analysis 2 - Comment out the code below to censor the data so the analysis uses all samples from all of time.
Hg_SS <- Hg_SS %>% filter (Sample_Year >= "2014")

# Calculate the average concentration (result) for each species by waterbody.
# The group_by code means that the subsequent calculations will be applied separately for each
# unique combination of Waterbody, Species_Codes, and FishType.
# The Num_obs=n()) line calculates the number of observations within each group of waterbody and species
# and creates a new variable called Num_Obs to store these counts i.e. it calculates the sample size for the ss
# advisory.
Hg_SS2 <- Hg_SS %>%
  group_by(Waterbody,Species_Codes, FishType) %>%
  mutate(Average_Result = mean(Result),
         Num_Obs = n())

# Obtain the unique rows based on the specified columns from the Hg_SS2 data frame.
# The distinct function keeps the distinct rows based on the Waterbody column while preserving all
# other columns specified in the .keep_all argument.
# This operation ensures that duplicate rows based on Waterbody are removed while
# preserving the values of other variables.
Hg_SS2= distinct(Hg_SS2, Waterbody, .keep_all = TRUE) %>%
  select(Waterbody, Species, Species_Codes, Average_Result, Units,
         Num_Obs, Commonly_Consumed,Length_Inches,Pred_Length, FishType)


# Assign meal frequency recommendations for site-specific advisories using the
# data frame with sufficient sample sizes
Hg_SS3=Hg_SS2 %>%
  mutate(GP_MealsPerMonth=case_when(Average_Result>0 & Average_Result <=.10 ~24,
                                    Average_Result>0.10 & Average_Result <=0.12 ~20,
                                    Average_Result>0.12 & Average_Result <=0.15 ~16,
                                    Average_Result>0.15 & Average_Result <=0.20 ~12,
                                    Average_Result>0.20 & Average_Result <=0.29 ~8,
                                    Average_Result>0.29 & Average_Result <=0.59 ~4,
                                    Average_Result>0.59 & Average_Result <=0.78 ~3,
                                    Average_Result>0.78 & Average_Result <=1.17 ~2,
                                    Average_Result>1.17 & Average_Result <=2.34 ~1,
                                    Average_Result>2.34 & Average_Result <=4.68 ~.5,
                                    Average_Result>4.68 & Average_Result <=9.37 ~.25,
                                    TRUE~0))



# ASSUME NO SITE-SPECIFIC ADVISORY BC THE EXISTING SS IS INVALID
# 1) Existing SS was formulated with a data set that had a lot of errors 
# 2) The fish species are not comparable because we no longer have small and big
# categories for all predator species.
# IN FUTURE UPDATES COMPARE TO EXISTING SS ADVISORIES


# Upload the existing statewide advisories data set.
# Always use the most recently updated statewide advisory.
library(readxl)
Hg_Statewide_Final_2024=read_excel("X:\\Shared drives\\_CDPHE TEEO TARA\\PFAS 🔥\\Data Integration and Assessment\\Fish\\FCAs\\Mercury FCAs\\Annual FCA updates\\2024 Update\\Hg_FCA_2024\\04_Output\\Hg_2024_Update\\Statewide_Hg_Advisory\\Hg_Statewide_Final_2024.xlsx")


# Merge the data frame with calculated site-specific advisories "Hg_SS3" with the Hg statewide advisory dataframe

# The select function is used to choose only the columns Species_Codes and GP_MealsPerMonth
# from Hg_Statewide_Final_2024.
# The left_join function is used to perform a left join between Hg_SS3 and the
# result of the select operation.
# The by = "Species_Codes" argument specifies the column used for the join, which is Species_Codes
# Rows from Hg_SS3 that have matching values in the Species_Codes column with
# Hg_Statewide_Final_2024 will have corresponding values from Hg_Statewide_Final_2024 included. Rows without a
# match will have NA values for the columns from Hg_Statewide_Final_2024
Hg_SS4 <- left_join(Hg_SS3, Hg_Statewide_Final_2024 %>% select(Species_Codes, GP_MealsPerMonth),
                    by = "Species_Codes")

# Rename the variables for GP_MealsPerMonth
Hg_SS4  <- Hg_SS4  %>%
  rename(GP_MealsPerMonthSS = GP_MealsPerMonth.x, GP_MealsPerMonthSW = GP_MealsPerMonth.y)

# Filter the data frame to subset species that are on the statewide advisory (<= 8 meals per month)
# and species that are not on the statewide advisory

Hg_SS5a_SW <- Hg_SS4 %>% filter(GP_MealsPerMonthSW <=8) # Species that have a statewide advisory

Hg_SS5b_NoSW <- Hg_SS4 %>% filter(GP_MealsPerMonthSW > 8) # Species that DO NOT have a statewide advisory


# FOR SPECIES WITH A STATEWIDE ADVISORY (<= 8 meals per month)

# Create a new variable to check if the site-specific advisory for a species is more stringent than the statewide advisory.

# The group_by function groups the data by the variable Species_Codes.The subsequent operations will
# be applied separately within each group
# The value of this variable is determined based on whether the corresponding value of GP_MealsPerMonthSS is
# less than the corresponding value of GP_MealsPerMonthSW within each group defined by Species_Codes. 
# If true, the value of the variable New_SS is "Yes"; otherwise, it's "No".

Hg_SS6_SW <- Hg_SS5a_SW %>%           
  group_by(Species_Codes) %>%
  mutate(New_SS = case_when(
    GP_MealsPerMonthSS < GP_MealsPerMonthSW ~ "Yes",
    TRUE ~ "No"
  )) 


# FOR SPECIES WITHOUT A STATEWIDE ADVISORY

# Use a variable called New_SS to determine whether species will have a new site-specific advisory based
# whether species meal recs  are <= 8 meals/month

Hg_SS6_NoSW <- Hg_SS5b_NoSW %>%
  mutate(New_SS = case_when(
    GP_MealsPerMonthSS <= 8 ~ "Yes",
    TRUE ~ "No"
  ))


# Merge all final data frames into one data frame that has a variable New_SS indicating
# when a species at a waterbody has a new or updated site-specific advisory.

# There should be two data frames merged:
# (1) Data frame for species with an existing statewide advisory.
# Hg_SS6_SW

# (2) Data frame for species without a statewide advisory.
# Hg_SS6_NoSW

Hg_GP_SS_Merged <- bind_rows(Hg_SS6_SW, Hg_SS6_NoSW)


# Merge the data frame "Hg_GP_SS_Merged" with the existing advisories list
# in order to evaluate which advisories are new, updated, or removed. In future advisories
# this merge will occur earlier because existing ss will be used to determine the updated advisories.

# Upload the existing advisories dataset
# This will change every year but always needs to include all existing advisories.
# Upload new version!!!
Hg_Existing_SSAdvisories=read_excel("X:\\Shared drives\\_CDPHE TEEO TARA\\PFAS 🔥\\Data Integration and Assessment\\Fish\\FCAs\\Mercury FCAs\\Annual FCA updates\\2024 Update\\Hg_FCA_2024\\01_Raw_Data\\Hg_Existing_SS.xlsx")

# Filter the data frame for the General Population
Hg_Existing_SSAdvisories %>% select(ExistingSS_Size) %>% distinct()

Hg_ExistingSS <- Hg_Existing_SSAdvisories %>% filter(Population == "General population")

# Select the most stringent meal recommendation for each population when a species
# has more than 1 size category. This step can be removed in future iterations.
# This step is only necessary in the 2024 update.
Hg_ExistingSS <- Hg_ExistingSS %>% 
  group_by(Waterbody, Species, Existing_SS_Advisory, Population) %>%
  summarize(MealsMonth_ExistingSS = min(MealsMonth_ExistingSS)) %>%
  arrange(Waterbody, Species)

# Merge the existing site-specific advisory data frame with with Hg_GP_SS_Merged
Hg_GP_SS_Merged2 <- left_join(Hg_GP_SS_Merged, Hg_ExistingSS %>% select( Waterbody, Species, Population, MealsMonth_ExistingSS, Existing_SS_Advisory),
                              by = c("Waterbody", "Species"))




