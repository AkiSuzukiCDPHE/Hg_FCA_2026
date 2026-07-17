
library(ggplot2)
library(dplyr)
library(readxl)

# Importing the final dataset with TAC recommendations####

# Locate the directory
getwd()

# Import the new data
HgData_Clean_All_Data <- read_excel("03_Clean_Data/Hg_CleanedMaster_2026.xlsx")


# TAC Plots Children ####


# Filter for the waterbodies and species with recommendations for children that need to be discussed at the TAC
# Filter for the censored data unless the discussion includes data with combined data
Hg_SS_TAC_Children <- HgData_Clean_All_Data %>% filter(Sample_Year >= 2016) |>
  filter((Waterbody == "Grand Lake" &
            Species  == "Brown Trout") |
           (Waterbody == "Lake Nighthorse" &
              Species == "Rainbow Trout"
           )
  )

# Calculate the average concentration (result) for each species by waterbody.
Hg_SS_TAC_Children2 <- Hg_SS_TAC_Children %>%
  group_by(Waterbody, Species) %>%
  mutate(Average_Result = mean(Result), Num_Obs = n())



# Create a combined X-axis column with a newline character (\n)
Hg_SS_TAC_Children2 <- Hg_SS_TAC_Children2%>%
  mutate(x_label = paste(Species, Num_Obs, Waterbody, sep = "\n"))



ggplot(Hg_SS_TAC_Children2, aes(x = x_label)) +
  # 1. Plot individual concentration points (blue circles)
  geom_point(aes(y = Result), color = "#808080", size = 3, alpha = 0.7) +
  
  # 2. Plot the average results (green diamonds)
  geom_point(aes(y = Average_Result), color = "#2a2a2a", shape = 18, size = 5) +
  
  # 3. Add horizontal threshold lines

  # geom_hline(yintercept = 1.04, color = "#4BACC6", linewidth = 1) + # 0.25 meals/month
  geom_hline(yintercept = 0.52, color = "#e40b0b", linewidth = 1) + # 0.5 meal/month
  geom_hline(yintercept = 0.26, color = "#fd9e02", linewidth = 1) + # 1 meal per month
  geom_hline(yintercept = 0.13, color = "#ffb703", linewidth = 1) + # 2 meal per month
  geom_hline(yintercept = 0.09, color = "#126782", linewidth = 1) + # 3 meal per month


  
  # 4. Add threshold text labels on the left side
  # annotate("text", x = 0.5, y = 1.20, label = "DO NOT EAT",  hjust = 0, size = 3.5) +
  # annotate("text", x = 0.5, y = .99, label = "0.25 meal/month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = 0.3, label = "0.5 meal/month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = .20, label = "1 meal/month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = .11, label = "2 meals per month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = .05, label = ">=3 meals per month",  hjust = 0, size = 3.5) +
  
  
  
  # 5. Formatting axes and limits
  scale_y_continuous(limits = c(0, 0.4), breaks = seq(0, 0.4, by = 0.2)) +
  labs(x = NULL, y = NULL) + # Removes default axis titles to match your clean look
  
  # 6. Styling the theme to match a clean grid
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "#E0E0E0"),
    panel.grid.major.y = element_line(color = "#E0E0E0"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11, color = "black", vjust = 0.5),
    axis.text.y = element_text(size = 11, color = "black")
  )




# TAC Plots Women ####


# Filter for the waterbodies and species with recommendations for Women that need to be discussed at the TAC
# Filter for the censored data unless the discussion includes data with combined data
Hg_SS_TAC_Women <- HgData_Clean_All_Data %>% filter(Sample_Year >= 2016) |>
  filter((Waterbody == "Puett Reservoir" &
            Species  == "Walleye") |
           (Waterbody == "Lake Nighthorse" &
              Species == "Rainbow Trout"
           ) | (Waterbody == "Vallecito Reservoir" &
           Species  == "Northern Pike")
  )

# Calculate the average concentration (result) for each species by waterbody.
Hg_SS_TAC_Women2 <- Hg_SS_TAC_Women %>%
  group_by(Waterbody, Species) %>%
  mutate(Average_Result = mean(Result), Num_Obs = n())



# Create a combined X-axis column with a newline character (\n)
Hg_SS_TAC_Women2 <- Hg_SS_TAC_Women2%>%
  mutate(x_label = paste(Species, Num_Obs, Waterbody, sep = "\n"))



ggplot(Hg_SS_TAC_Women2, aes(x = x_label)) +
  # 1. Plot individual concentration points (blue circles)
  geom_point(aes(y = Result), color = "#808080", size = 3, alpha = 0.7) +
  
  # 2. Plot the average results (green diamonds)
  geom_point(aes(y = Average_Result), color = "#2a2a2a", shape = 18, size = 5) +
  
  # 3. Add horizontal threshold lines
  
  # geom_hline(yintercept = 3.19, color = "#4BACC6", linewidth = 1) + # 0.25 meals/month
  geom_hline(yintercept = 1.59, color = "#e40b0b", linewidth = 1) + # 0.5 meal/month
  geom_hline(yintercept = 0.8, color = "#fd9e02", linewidth = 1) + # 1 meal per month
  geom_hline(yintercept = 0.4, color = "#ffb703", linewidth = 1) + # 2 meal per month
  geom_hline(yintercept = 0.27, color = "#126782", linewidth = 1) + # 3 meal per month
  geom_hline(yintercept = 0.2, color = "#219ebc", linewidth = 1) + # 4 meal per month
  geom_hline(yintercept = 0.1, color = "#8ecae6", linewidth = 1) + # 8 meal per month
  
  
  # 4. Add threshold text labels on the left side
  # annotate("text", x = 0.5, y = 1.20, label = "DO NOT EAT",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = 1.63, label = "0.25 meal/month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = 1.2, label = "0.5 meal/month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = .7, label = "1 meal/month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = .32, label = "2 meals per month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = .23, label = "3 meals per month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = .16, label = "4 meals per month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = .05, label = ">= 8 meals per month",  hjust = 0, size = 3.5) +
  
  
  # 5. Formatting axes and limits
  scale_y_continuous(limits = c(0, 1.63), breaks = seq(0, 1.63, by = 0.2)) +
  labs(x = NULL, y = NULL) + # Removes default axis titles to match your clean look
  
  # 6. Styling the theme to match a clean grid
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "#E0E0E0"),
    panel.grid.major.y = element_line(color = "#E0E0E0"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11, color = "black", vjust = 0.5),
    axis.text.y = element_text(size = 11, color = "black")
  )




# TAC Plots General Population ####


# Filter for the waterbodies and species with recommendations for Women that need to be discussed at the TAC
# Filter for the censored data unless the discussion includes data with combined data
Hg_SS_TAC_GP <- HgData_Clean_All_Data %>% filter(Sample_Year >= 2016) |>
  filter(Waterbody == "Puett Reservoir" &
            Species  == "Walleye") 

# Calculate the average concentration (result) for each species by waterbody.
Hg_SS_TAC_GP2 <- Hg_SS_TAC_GP %>%
  group_by(Waterbody, Species) %>%
  mutate(Average_Result = mean(Result), Num_Obs = n())



# Create a combined X-axis column with a newline character (\n)
Hg_SS_TAC_GP2 <- Hg_SS_TAC_GP2%>%
  mutate(x_label = paste(Species, Num_Obs, Waterbody, sep = "\n"))



ggplot(Hg_SS_TAC_GP2, aes(x = x_label)) +
  # 1. Plot individual concentration points (blue circles)
  geom_point(aes(y = Result), color = "#808080", size = 3, alpha = 0.7) +
  
  # 2. Plot the average results (green diamonds)
  geom_point(aes(y = Average_Result), color = "#2a2a2a", shape = 18, size = 5) +
  
  # 3. Add horizontal threshold lines
  
  # geom_hline(yintercept = 9.37, color = "#4BACC6", linewidth = 1) + # 0.25 meals/month
  geom_hline(yintercept = 4.68, color = "#e40b0b", linewidth = 1) + # 0.5 meal/month
  geom_hline(yintercept = 2.34, color = "#fd9e02", linewidth = 1) + # 1 meal per month
  geom_hline(yintercept = 1.17, color = "#ffb703", linewidth = 1) + # 2 meal per month
  geom_hline(yintercept = 0.78, color = "#126782", linewidth = 1) + # 3 meal per month
  geom_hline(yintercept = 0.59, color = "#219ebc", linewidth = 1) + # 4 meal per month
  geom_hline(yintercept = 0.29, color = "#8ecae6", linewidth = 1) + # 8 meal per month
  
  
  # 4. Add threshold text labels on the left side
  # annotate("text", x = 0.5, y = 1.20, label = "DO NOT EAT",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = 7, label = "0.25 meal/month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = 3, label = "0.5 meal/month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = 1.5, label = "1 meal/month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = 1, label = "2 meals per month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = .7, label = "3 meals per month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = .5, label = "4 meals per month",  hjust = 0, size = 3.5) +
  annotate("text", x = 0.5, y = .2, label = ">= 8 meals per month",  hjust = 0, size = 3.5) +
  
  
  # 5. Formatting axes and limits
  scale_y_continuous(limits = c(0, 1.63), breaks = seq(0, 1.63, by = 0.2)) +
  labs(x = NULL, y = NULL) + # Removes default axis titles to match your clean look
  
  # 6. Styling the theme to match a clean grid
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "#E0E0E0"),
    panel.grid.major.y = element_line(color = "#E0E0E0"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11, color = "black", vjust = 0.5),
    axis.text.y = element_text(size = 11, color = "black")
  )
