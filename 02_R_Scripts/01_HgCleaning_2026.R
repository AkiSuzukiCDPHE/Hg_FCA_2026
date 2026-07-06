# R SCRIPT FOR CLEANING THE NEW MERCURY DATA & MERGING WITH EXISTING DATA

# Importing new data ####

# importing xlsx files of raw data from the past year's sampling efforts 
library(readxl)
library(dplyr)

getwd()

# Import the new data
HgData <- read_excel("01_Raw_Data/2025_2024_FCA_Hg_Validated.xlsx") 


# Cleaning the new data ####

# Remove samples where tissue type = "O" or "E" (keep M & plug only).
HgData_1 <- HgData |>  filter(Tissue_Type %in% c('M','Plug'))

# Run a frequency table to make sure filtering for M and Plug worked
table(HgData_1$Tissue_Type)

# Recreate the values for species based on species code
# This catches any typos or issues in the raw data if the name was incorrect or if they lab labeled big and small fish sizes.
#4a. NPK & NPKB = Northern pike"
#4b. WALB & WAL = "Walleye"
#4c. SMBB & SMB = "Smallmouth bass"
#4d. LMBB & LMB = "Largemouth bass"
#4e. SAGB & SAG = Saugeye
#4f. NAT = "Cutthroat"
#4g. SRN = "Cutthroat"
#4h. RGN = "Cutthroat"
#4i. CRN = "Cutthroat"
#4j. RXN = "Rainbow trout"


library(dplyr)
HgData_2 <- HgData_1 %>%
  mutate(Species = case_when(Species_Code == "SMB" ~ "Smallmouth Bass",
                             Species_Code == "SMBB" ~ 'Smallmouth Bass',
                             Species_Code == "LMB" ~ 'Largemouth Bass',
                             Species_Code == "LMBB" ~ 'Largemouth Bass',
                             Species_Code == "TGM" ~ 'Tiger Muskie',
                             Species_Code == "NPK" ~ 'Northern Pike',
                             Species_Code == "NPKB" ~ 'Northern Pike',
                             Species_Code == "WAL" ~ 'Walleye',
                             Species_Code == "WALB" ~ 'Walleye',
                             Species_Code == "BCR" ~ "Black Crappie",
                             Species_Code == "BGL" ~ "Bluegill",
                             Species_Code == "BBH" ~ "Black Bullhead",
                             Species_Code == "CPP" ~ 'Common Carp',
                             Species_Code == "CCF" ~ 'Channel Catfish',
                             Species_Code == "MAC" ~ "Lake Trout(Mackinaw)",
                             Species_Code == "SAG" ~ 'Saugeye',
                             Species_Code == "SAGB" ~ "Saugeye",
                             Species_Code == "SPL" ~ "Splake",
                             Species_Code == "SBS" ~ "Striped Bass",
                             Species_Code == "SNF" ~ "Green Sunfish",
                             Species_Code == "WBA" ~ "White Bass",
                             Species_Code == "SXW" ~ "Wipe",
                             Species_Code == "WHS" ~ "White Sucker",
                             Species_Code == "YPE" ~ "Yellow Perch",
                             Species_Code == "RXC" ~ "Rainbow Trout x Cutthroat",
                             Species_Code == "RXN" ~ "Rainbow Trout",
                             Species_Code == "SRN" ~ "Cutthroat",
                             Species_Code == "RGN" ~ "Cuttrhoat",
                             Species_Code == "BRK" ~ "Brook Trout",
                             Species_Code == "RBT" ~ "Rainbow Trout",
                             Species_Code == "KOK" ~ "Kokanee",
                             Species_Code == "LOC" ~ "Brown Trout",
                             Species_Code == "DRM" ~ "Freshwater Drum",
                             Species_Code == "PKS" ~ "Pumpkinseed",
                             Species_Code == "SGR" ~ "Sauger",
                             Species_Code == "WCR" ~ "White Crappie",
                             Species_Code == "NAT" ~ "Cutthroat",
                             Species_Code == "CRN" ~ "Cutthroat",
                             Species_Code == "LGS" ~ "Longnose Sucker",
                             Species_Code == "GRA" ~ "Arctic Grayling",
                             Species_Code == "SQF" ~ "Colorado Pikeminnow",
                             Species_Code == "CFI" ~ "Crayfish",
                             Species_Code == "FMS" ~ "Flannelmouth Sucker",
                             Species_Code == "GSD" ~ "Gizzard Shed",
                             Species_Code == "GSF" ~ "Golden Shiner maybe (GSH)",
                             Species_Code == "HBG" ~ "Hybrid Bluegill",
                             Species_Code == "SPB" ~ "Spotted Bass",
                             Species_Code == "TRT" ~ "Trout - unspecified",
                             Species_Code == "SXX" ~ "White bass x wiper backcross",
TRUE ~ Species))
                          


# Rename water bodies in the new data set where there is a discrepancy with the old master data set (manually check with previous years clean dataset)
# This should not be common as the lab uses the data template
table(HgData_2$Waterbody)

# Rename waterbodies if needed
# HgData_2 <- HgData_1 %>% mutate(Waterbody = case_when(Waterbody == "Jumbo Annex" ~ "Jumbo Lake"),
#           TRUE ~ Waterbody)


# Lump subspecies together in the species code
HgData_3=HgData_2 %>%
  mutate(Species_Code = case_when(Species_Code == "SMBB" ~ "SMB",
                                   Species_Code == "LMBB" ~ 'LMB',
                                   Species_Code == "NPKB" ~ 'NPK',
                                   Species_Code == "WALB" ~ 'WAL',
                                   Species_Code == "SAGB" ~ "SAG",
                                   Species_Code == "RXN" ~ "RBT",
                                   Species_Code == "SRN" ~ "NAT",
                                   Species_Code == "RGN" ~ "NAT",
                                   Species_Code == "CRN" ~ "NAT",
                                   TRUE~Species_Code))




# Replace non-detect values with the MDL
  HgData_4 <- HgData_3 %>%
  mutate(Result=case_when(Qualifier %in% c("<", "BDL") ~ MDL,
                                    TRUE~ Result))
  

  
# Merge with Master Dataset #### 
  
  # Import the master dataset from the previous year (already cleaned). This will be in the 03_Clean_Data folder of the previous update
  HgData_Master <- read_excel("03_Clean_Data/Hg_CleanedMaster_2024.xlsx")
  

  # check variable names in new data and master dataset to make sure they are the same
  colnames(HgData_Master)
  colnames(HgData_4)
  
  
  # # Step 1: Rename variables with long names or names that are not the same as the master dataset you will eventually merge the new data with.
  # # WILL REQUIRE MAJOR UPDATE WHEN LAB STARTS USING VARIABLE NAMES IN FISH DATA TEMPLATE
  # HgData <- HgData %>% rename ("Result" ="Hg (mg/kg)", "Waterbody" = "Waterbody1")
  
  
# Step 11: Reconcile different data types in the master vs new dataset
  HgData_Master$`Length_mm`<- as.numeric(HgData_Master$`Length_mm`)
  HgData_Master$`Weight (g)`<- as.numeric(HgData_Master$`Weight (g)`)
  HgData_Master$Length_Inches<- as.numeric(HgData_Master$Length_Inches)
  HgData_Master$Length_Inches<- as.numeric(HgData_Master$Length_Inches)
  
  
# Step 12: Merge master dataset with new cleaned dataframe
# Combine dataframes by appending rows

  HgData_Merged <- bind_rows(HgData_Master, HgData_4, .id = "Source") 
  
  colnames(HgData_Merged)
  
  
  HgData_Merged1 <-  HgData_Merged |> select(-c( `CAS Number`, `Field Notes`, `Percent Recovered`, Analysis_Date, Lab_Method, Report_Date, `Lab Comment`,
                                             Dissolved_Oxygen, `Weight (g)`, pH, `RL/PQL`, Collection_Date, Conductivity, Prep_Date, Water_Temperature))
  
  
# Filter out crayfish and trout unspecified because these Species_Code should not be included in the cleaned data
# Trout-unspecified is not a Species_Code and Crayfish is not a fish.
  HgData_Merged2 <- subset(HgData_Merged1, !Species %in% c("Crayfish", "Trout - unspecified"))
  
  
  
# Create new column denoting whether a Species_Code is in the existing statewide advisory.
# Double check these are still true each year using the FCA dashboard's General advisory
  
  HgData_Merged3 <- HgData_Merged2 %>%
    mutate(Statewide_Advisory = case_when(
      Species %in% c(
        "Arctic Grayling",
        "Colorado Pikeminnow",
        "Rainbow Trout x Cutthroat",
        "Spotted Bass",
        "White bass x Wiper backcross",
        "Pumpkinseed",
        "Hybrid Bluegill")
        ~ "No",
        TRUE ~ "Yes"
      )
    )
    
  
  
# Create a new variable for whether a fish Species_Code is commonly consumed. This is unlikely to chnage often.
  HgData_Clean <- HgData_Merged3 %>%
    mutate(Commonly_Consumed = case_when(Species_Code %in% c("SQF", "CFI","FMS","GSD","GSF") ~ "No",
                                         TRUE ~ "Yes"))
  
# This is your cleaned data frame you will use in subsequent scripts.
  HgData_Clean
  
# Export the data frame as a cleaned master dataset ####
# Title with the following format: “Hg_CleanedMaster_CurrentYear”

  
# Export to the cleaned master data folder on the drive. Change the output year each time. If exporting doesnt work copy it into the folder
library("writexl")
write_xlsx(HgData_Clean, ("03_Clean_Data/Hg_CleanedMaster_2026.xlsx"))



  
  