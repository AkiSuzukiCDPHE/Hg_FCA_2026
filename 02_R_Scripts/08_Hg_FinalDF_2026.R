# FINAL MERCURY SITE-SPECIFIC ADVISORY SCRIPT

library(dplyr)
library(readxl)

# Turn off scientific notation
options(scipen = 999)

# Final cleaning steps for final site-specific output ####

# Change filepath
HgSS_Final_AllPops_Censored <- read_excel("04_Output/2026_Hg_SS_Censored.xlsx")


# Filter out species that do not meet sample size requirements and have not been selected for manual review.
# This sample size will change if the power analysis is rerun and produces a different result.
Hg_SS_FCAs_Final <- HgSS_Final_AllPops_Censored %>%
  filter(!(Num_Obs<11 & Manual_Review != "Yes"))


# Subset to df with all new, updated, or potentially lifted advisories
Hg_SS_FCAs_Final_1 <- Hg_SS_FCAs_Final |>  filter(Rec %in% c(
  "Same as Statewide - Consider removing SS advisory with TAC",
  "Adopt SS advisory (More stringent than statewide)",
  "Adopt updated (Less Stringent than existing SS)",
  "Issue a new site-specific FCA",
  "Consider removing SS advisory with TAC",
  "Review manually"
))


# Upload the CPW Regional Waterbodies
CPWRegions <- read_excel("01_Raw_Data/Lakes_with_CPWAreaRegion_County.xlsx")
 
# Merge the regional waterbodies with the advisories
Hg_SS_FCAs_Final_CPW_Regions <- merge(Hg_SS_FCAs_Final_1, CPWRegions[, c("Waterbody","REGION")], by = "Waterbody", all.x = TRUE)

# Make sure all rows are distinct
Hg_SS_FCAs_Final_CPW_Regions1 <- Hg_SS_FCAs_Final_CPW_Regions %>%
  distinct(Waterbody, Species, Average_Result, Population, .keep_all = TRUE) |> rename (CPW_Region = REGION)

# Rename columns to make them more legible and reorder variables
Hg_SS_FCAs_Final_CPW_Regions2 <- Hg_SS_FCAs_Final_CPW_Regions1 %>% rename(
  c(
    `Sample N` = Num_Obs,
    Size = FishType,
    `Proposed (meals per month)` = MealsPerMonth,
    `Existing site-specific (meals per month)` = Current_SS_Per_Month,
    `Existing site-specific status` = SS_Status,
    `Statewide (meals per month)` = Statewide_Per_Month,
    `Statewide status` = State_Status,
    Recommendation = Rec))  |> select (Waterbody:Species_Code, Commonly_Consumed, Average_Result:`Sample N`, Length_Inches:CPW_Region) |> mutate(Size=if_else(is.na(Size), "Any", Size))
      


# Export the FINAL dataset ####

library("writexl")
write_xlsx(Hg_SS_FCAs_Final_CPW_Regions2, "04_Output/2026_Hg_SS_Final_FCAs.xlsx")


