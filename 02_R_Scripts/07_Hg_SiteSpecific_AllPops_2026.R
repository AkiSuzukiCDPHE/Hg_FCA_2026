# R SCRIPT FOR MERCURY SITE-SPECIFIC UPDATES TO COMBINE ALL POPULATIONS AND EXPORT ADVISORIES

# This R Script will combine the GP, WCBA, and Child site-specific data into one dataframe
# and rename, reorder, and delete extraneous variables.

# Merge the data  and transform to long ####

library(dplyr)
# merge all three pops
FCAs_2026 <-  Advisories_GP |> left_join(
  Advisories_WCBA,
  by = c(
    "Waterbody",
    "Species_Code",
    "Species",
    "Average_Result",
    "Units",
    "Num_Obs",
    "FishType",
    "Current_Fishtype",
    "Commonly_Consumed",
    "Length_Inches",
    "Pred_Length"
  )
) |> left_join(
  Advisories_Children,
  by = c(
    "Waterbody",
    "Species_Code",
    "Species",
    "Average_Result",
    "Units",
    "Num_Obs",
    "FishType",
    "Current_Fishtype",
    "Commonly_Consumed",
    "Length_Inches",
    "Pred_Length"
  )
) |>  relocate(Current_Fishtype, .after = FishType)




library(tidyr)
# Pivot longer
FCAs_2026_Long <- FCAs_2026 %>%
  pivot_longer(
    # Do not pivot Waterbody through FishType
    cols = !(Waterbody:Current_Fishtype),
    
    # Use names_pattern instead of names_sep
    # This captures the Group first, then the Variable name, regardless of underscores
    names_to = c("Population", ".value"),
    names_pattern = "(GP|WCBA|Children)_(.*)"
  )


# Analysis 1 CENSORED DATA ####

# The data frame below has FCA recommendations for all populations and includes lifted, updated, and
# brand new advisories, as well as species that don't require an advisory and never had one.
# This semi-final dataset is used for manual review to determine advisories and decide on when to combine data.
# Always use this format: "HgSS_Final_AllPops_Censored20XX" or "HgSS_Final_AllPops_Combined20XX"

HgSS_AllPops_Censored2026 <- FCAs_2026_Long

#  Exporting the final CENSORED dataframe
#  Change file paths

# library(writexl)
# write_xlsx(HgSS_AllPops_Censored2026,"04_Output/2026_Hg_SS_Censored.xlsx")



# Analysis 2 COMBINED DATA FOR SELECT WATERBODIES/SPECIES ####

# Data should be filtered for only waterbodies and species that were flagged as “Yes” for combining data during the manual review.
# FCAs_2026_Long_CombinedData <- FCAs_2026_Long %>%
#   filter(
#     (Waterbody == "Yamcolo Reservoir" &
#        Species == "Mountain Whitefish") |
#       (Waterbody == "Ruedi Reservoir" & Species == "Yellow Perch") |
#       (Waterbody == "Rocky Mountain Lake" &
#          Species == "Largemouth Bass") |
#       (Waterbody == "Puett Reservoir" & Species == "Walleye") |
#       (Waterbody == "Horsetooth Reservoir" &
#          Species == "Smallmouth Bass") |
#       (Waterbody == "Big Meadows Reservoir" &
#          Species == "Brook Trout") |
#       (
#         Waterbody == "Big Creek Reservoir" &
#           Species == "Lake Trout(Mackinaw)"
#       )
#   )

#  Rename dataset

# HgSS_Final_AllPops_Combined2026 <- FCAs_2026_Long_CombinedData

# # Exporting the final COMBINED dataframe.
# # Change file paths


# library("writexl")
# write_xlsx(HgSS_Final_AllPops_Combined2026,
#            "04_Output/2026_Hg_SS_Combined.xlsx")
