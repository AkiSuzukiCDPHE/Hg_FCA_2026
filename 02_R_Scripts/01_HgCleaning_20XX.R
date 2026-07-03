# R SCRIPT FOR CLEANING THE NEW MERCURY DATA & MERGING WITH EXISTING DATA

# importing xlsx files of raw data from the past year's sampling efforts 
install.packages("readxl")
library(readxl)

# Import the new data
HgData <- read_excel("X:\\Shared drives\\_CDPHE TEEO TARA\\PFAS 🔥\\Data Integration and Assessment\\Fish\\FCAs\\Mercury FCAs\\Annual FCA updates\\2024 Update\\Hg_FCA_2024\\01_Raw_Data\\HgData_Raw_2023.xlsx")

# Import the master dataset from the previous year (already cleaned)
HgData_Master <- read_excel("X:\\Shared drives\\_CDPHE TEEO TARA\\PFAS 🔥\\Data Integration and Assessment\\Fish\\FCAs\\Mercury FCAs\\Annual FCA updates\\2024 Update\\Hg_FCA_2024\\03_Clean_Data\\Hg_CleanedMaster_2022.xlsx")

# check variable names in new data and master dataset to make sure they are the same
colnames(HgData_Master)
colnames(HgData)

# Step 1: Rename variables with long names or names that are not the same as the master dataset you will eventually merge the new data with.
# WILL REQUIRE MAJOR UPDATE WHEN LAB STARTS USING VARIABLE NAMES IN FISH DATA TEMPLATE
HgData <- HgData %>% rename ("Result" ="Hg (mg/kg)", "Waterbody" = "Waterbody1")

# Rename variables in the master dataset if necessary
HgData_Master <- HgData_Master %>% rename ("MDL" = "MDL_PQL")


# Step 2: Remove samples where tissue type = "O" or "E" (keep M & plug only)
HgData_1 <- HgData[HgData$Tissue_Type %in% c('M','Plug'),]

# Run a frequency table to make sure filtering for M and Plug worked
table(HgData_1$Tissue_Type)

# Step 3: Create a new column titled "species" and populate with species names, including lumping species as specified
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
  mutate(Species = case_when(Species1 == "SMB" ~ "Smallmouth Bass",
                             Species1 == "SMBB" ~ 'Smallmouth Bass',
                             Species1 == "LMB" ~ 'Largemouth Bass',
                             Species1 == "LMBB" ~ 'Largemouth Bass',
                             Species1 == "TGM" ~ 'Tiger Muskie',
                             Species1 == "NPK" ~ 'Northern Pike',
                             Species1 == "NPKB" ~ 'Northern Pike',
                             Species1 == "WAL" ~ 'Walleye',
                             Species1 == "WALB" ~ 'Walleye',
                             Species1 == "BCR" ~ "Black Crappie",
                             Species1 == "BGL" ~ "Bluegill",
                             Species1 == "BBH" ~ "Black Bullhead",
                             Species1 == "CPP" ~ 'Common Carp',
                             Species1 == "CCF" ~ 'Channel Catfish',
                             Species1 == "MAC" ~ "Lake Trout(Mackinaw)",
                             Species1 == "SAG" ~ 'Saugeye',
                             Species1 == "SAGB" ~ "Saugeye",
                             Species1 == "SPL" ~ "Splake",
                             Species1 == "SBS" ~ "Striped Bass",
                             Species1 == "SNF" ~ "Green Sunfish",
                             Species1 == "WBA" ~ "White Bass",
                             Species1 == "SXW" ~ "Wipe",
                             Species1 == "WHS" ~ "White Sucker",
                             Species1 == "YPE" ~ "Yellow Perch",
                             Species1 == "RXC" ~ "Rainbow Trout x Cutthroat",
                             Species1 == "RXN" ~ "Rainbow Trout",
                             Species1 == "SRN" ~ "Cutthroat",
                             Species1 == "RGN" ~ "Cuttrhoat",
                             Species1 == "BRK" ~ "Brook Trout",
                             Species1 == "RBT" ~ "Rainbow Trout",
                             Species1 == "KOK" ~ "Kokanee",
                             Species1 == "LOC" ~ "Brown Trout",
                             Species1 == "DRM" ~ "Freshwater Drum",
                             Species1 == "PKS" ~ "Pumpkinseed",
                             Species1 == "SGR" ~ "Sauger",
                             Species1 == "WCR" ~ "White Crappie",
                             Species1 == "NAT" ~ "Cutthroat",
                             Species1 == "CRN" ~ "Cutthroat",
                             Species1 == "LGS" ~ "Longnose Sucker",
                             Species1 == "GRA" ~ "Arctic Grayling",
                             Species1 == "SQF" ~ "Colorado Pikeminnow",
                             Species1 == "CFI" ~ "Crayfish",
                             Species1 == "FMS" ~ "Flannelmouth Sucker",
                             Species1 == "GSD" ~ "Gizzard Shed",
                             Species1 == "GSF" ~ "Golden Shiner maybe (GSH)",
                             Species1 == "HBG" ~ "Hybrid Bluegill",
                             Species1 == "SPB" ~ "Spotted Bass",
                             Species1 == "TRT" ~ "Trout - unspecified",
                             Species1 == "SXX" ~ "White bass x wiper backcross"))
                          

# Confirm that new variable with full specie's names was correctly added to the dataframe
table(HgData_2$Species1)

# Step 4:Rename waterbodies in the new dataset where there is a discrepancy with the old master dataset
#  Do this by generating a table of the waterbody names and searching them in the master dataset

table(HgData_2$Waterbody)

# Example code to rename waterbodies
# HgData_2 <- HgData_1 %>% mutate(Waterbody = case_when(Waterbody == "Jumbo Annex" ~ "Jumbo Lake"),
#           TRUE ~ Waterbody)


# Step 5:Create a new variable, Species_Codes, with fish subspecies lumped together.
HgData_3=HgData_3 %>%
  mutate(Species_Codes = case_when(Species1 == "SMBB" ~ "SMB",
                                   Species1 == "LMBB" ~ 'LMB',
                                   Species1 == "NPKB" ~ 'NPK',
                                   Species1 == "WALB" ~ 'WAL',
                                   Species1 == "SAGB" ~ "SAG",
                                   Species1 == "RXN" ~ "RBT",
                                   Species1 == "SRN" ~ "NAT",
                                   Species1 == "RGN" ~ "NAT",
                                   Species1 == "CRN" ~ "NAT",
                                   TRUE~Species1))


# Step 6:Create new column denoting whether a species is in the existing statewide advisory. 
HgData_3 <- HgData_3 %>%
  mutate(In_Existing_Advisory = case_when(Species1 %in% c("GRA", "SQF", "CFI", "FMS","GSD","GSF","HBG", 
                                                    "PKS", "SPB","TRT","SXX") ~ "No",
                                          TRUE ~ "Yes"))


# Step 7: Create a new variable for whether a fish species is commonly consumed
  HgData_4 <- HgData_3 %>%
  mutate(Commonly_Consumed = case_when(Species1 %in% c("SQF", "CFI","FMS","GSD","GSF") ~ "No",
                                      TRUE ~ "Yes"))


# Step 7: Replace non-detect values with the MDL/PQL. This step uses the case_when function to
# replace values of one existing variable based on values of another existing variable.
  HgData_5 <- HgData_4 %>%
  mutate(Result=case_when(Qualifier == "<"~ MDL_PQL,
                                    TRUE~ Result))
  
  # Step 8: Create a new variables if missing from new dataset but present in the master data
  HgData_5$Sample_Year <- 2023
  HgData_5$Tissue_Type <- "M"
  HgData_5$Units <- "mg/Kg"
  
  
# Step 9: Create a new variable for fish length in inches based on the variable for length in mm
  HgData_5$Length_Inches <- HgData_5$`Length (mm)`/25.4
  
# Step 10: Delete unnecessary columns
  HgData_5 <- select( HgData_5,- Species1, - Data_Cleaning_Notes)
  

# Step 11: Reconcile different data types in the master vs new dataset
  class(HgData_Master$`Length (mm)`)
  HgData_Master$`Length (mm)`<- as.numeric(HgData_Master$`Length (mm)`)
  HgData_Master$`Weight (g)`<- as.numeric(HgData_Master$`Weight (g)`)
  HgData_Master$Length_Inches<- as.numeric(HgData_Master$Length_Inches)
  HgData_Master$Length_Inches<- as.numeric(HgData_Master$Length_Inches)
  
  
# Step 12: Merge master dataset with new cleaned dataframe
# Combine dataframes by appending rows
  HgData_5 <- bind_rows(HgData_Master, HgData_5, .id = "Source")
  

# Step 13: filter out crayfish and trout unspecified because these species should not be included in the cleaned data
# Trout-unspecified is not a species and Crayfish is not a fish.
  HgData_Clean <- subset(HgData_5, !Species %in% c("Crayfish", "Trout - unspecified"))
  
  
# This is your cleaned data frame you will use in subsequent scripts.
  HgData_Clean
  
# Export the data frame as a cleaned master dataset for next year's analysis. 
# Title with the following format: “Hg_CleanedMaster_CurrentYear”
  
# library("writexl")
# write_xlsx(HgData_Clean,"X:\\Shared drives\\_CDPHE TEEO TARA\\PFAS 🔥\\Data Integration and Assessment\\Fish\\FCAs\\Mercury FCAs\\Annual FCA updates\\2024 Update\\Hg_FCA_2024\\03_Clean_Data\\Hg_CleanedMaster_20XX.xlsx")
 

  

  
  