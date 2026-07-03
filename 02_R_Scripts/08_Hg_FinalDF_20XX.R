# FINAL MERCURY SITE-SPECIFIC ADVISORY SCRIPT

library(dplyr)
library(readxl)

# Turn off scientific notation
options(scipen = 999)

# Upload the censored semi-final dataset as a csv.
# Change filepath
# This will always be in the following format: "HgSS_Final_AllPops_Censored20XX"

HgSS_Final_AllPops_Censored20XX <- read_excel("C:/Users/oasuzuki/Documents/R/FCAs/Hg_FCA/04_Output/Hg_2024_Update/Site-specific_Hg_Advisory/SS_TimeNOTCensored_USETHIS/Semi_Final_Dataset/HgSS_Final_AllPops_Censored20XX.xlsx")


# Filter out species that do not meet sample size requirements and have not been selected for manual review.
# This sample size will change if the power analysis is rerun and produces a different result.
HgSS_Final_AllPops1 <- HgSS_Final_AllPops_Censored20XX %>%
  filter(!(Num_Obs<11 & Manual_Review != "Yes"))


# Subset to df with all new and updated ss advisories for all populations
HgSS_Final_AllPops_Yes <- HgSS_Final_AllPops1[HgSS_Final_AllPops1$GP_SS_Status == "Yes" 
                                     | HgSS_Final_AllPops1$WCBA_SS_Status == "Yes" 
                                     | HgSS_Final_AllPops1$Child_SS_Status == "Yes"
                                      ,]

# Subset to df with all lifted or loosened advisories for all populations. I.e. when 
# the updated SS advisory is less stringent than the existing ss advisory.

HgSS_Final_AllPops_LiftedLoosened <- HgSS_Final_AllPops1[
                  HgSS_Final_AllPops1$GP_Meals_SS > HgSS_Final_AllPops1$GP_Existing_SS
                | HgSS_Final_AllPops1$WCBA_Meals_SS > HgSS_Final_AllPops1$WCBA_Existing_SS
                | HgSS_Final_AllPops1$Child_Meals_SS > HgSS_Final_AllPops1$Child_Existing_SS
                ,]

# Remove rows where all column values = "NA"
HgSS_Final_AllPops_LiftedLoosened2 <-filter(HgSS_Final_AllPops_LiftedLoosened, rowSums(is.na(HgSS_Final_AllPops_LiftedLoosened)) != ncol(HgSS_Final_AllPops_LiftedLoosened))


# Subset again for only advisories that are lifted bc the ss >= the sw advisory
HgSS_Final_AllPops_Lifted <- HgSS_Final_AllPops_LiftedLoosened2 %>% 
filter(GP_Meals_SS >= GP_Meals_SW | WCBA_Meals_SS >= WCBA_Meals_SW |
Child_Meals_SS >= Child_Meals_SW) 


# Merge the lifted advisories with the updated/new advisory dataset
Hg_SS_LiftedUpdatedNew <- bind_rows(HgSS_Final_AllPops_Yes, HgSS_Final_AllPops_Lifted)


# Subset to remove duplicated rows from Hg_SS_LiftedUpdatedNew
Hg_SS_LiftedUpdatedNew1 <- Hg_SS_LiftedUpdatedNew %>%
  distinct(Waterbody, Species, Average_Result, .keep_all = TRUE)

# # Upload the CPW Regional Waterbodies
# CPWRegions <- read_excel("C:/Users/oasuzuki/Documents/R/FCAs/Hg_FCA/01_Raw_Data/Lakes_with_CPWAreaRegion_County.xlsx")
# 
# # Merge the regional waterbodies with the Hg_SS_LiftedUpdatedNew1 dataset.
# Hg_SS_LiftedUpdatedNew2 <- merge(Hg_SS_LiftedUpdatedNew1, CPWRegions[, c("Waterbody","REGION")], by = "Waterbody", all.x = TRUE)

# Make sure all rows are distinct
Hg_SS_LiftedUpdatedNew_FINAL <- Hg_SS_LiftedUpdatedNew1 %>%
  distinct(Waterbody, Species, Average_Result, .keep_all = TRUE)


# Export the FINAL dataset!
# Follow next steps on analysis directions document

# library("writexl")
# write_xlsx(Hg_SS_LiftedUpdatedNew_FINAL, "C:/Users/oasuzuki/Documents/R/FCAs/Hg_FCA/04_Output/Hg_2024_Update/Site-specific_Hg_Advisory/SS_TimeNOTCensored_USETHIS/Final_Dataset//Hg_SS_LiftedUpdatedNew_FINAL_2024.xlsx")


