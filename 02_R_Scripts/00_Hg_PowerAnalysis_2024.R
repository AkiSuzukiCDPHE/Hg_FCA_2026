library(ggplot2)
library(dplyr)
library(broom)
library(readxl)
library(readr)
library("tidyverse")


# Upload the THE CLEANED MERCURY DATASET
# This power analysis should be conducted every 10 years
# The most recent power analysis was conducted in 2024 using the "Hg_CleanedMaster_2022" dataset
# The next power analysis should be conducted in 2034

HgData_Clean_Power <- read_excel("03_Clean_Data/Hg_CleanedMaster_2026.xlsx")


# POWER ANALYSIS FOR MERCURY DATA

# STEP 1: Calculate the effect size
# Effect size =(MeanH1-MeanH0)/SD

# MeanH0 = Screening value/threshold for issuing a site-specific advisory (8 meal per month FCLG)
# For Hg this is .091 mg/ mercury/kg fish
# For PFOS this is .91 ng/g

MeanH0 <- .091

# MeanH1 = Mean fish tissue concentration by Waterbody by species

# Standard deviation = Standard deviation of fish tissue concentrations by waterbody by species.


# Calculating MeanH1

library(dplyr)


# Combine mean and sd calculations into one step
summary_stats <- HgData_Clean_Power %>%
  group_by(Waterbody, Species) %>%
  summarise(
    mean_variable = mean(Result, na.rm = TRUE),
    Standard_Dev  = sd(Result, na.rm = TRUE),      # Fixed: sd() instead of std_dev()
    .groups = "drop"
  )

# Extract dynamic medians
MeanH1 <- median(summary_stats$mean_variable, na.rm = TRUE)
Standard_Deviation <- median(summary_stats$Standard_Dev, na.rm = TRUE)



library(pwr)

# Effect size =(MeanH1-MeanH0)/SD =

Effect_Size <- (MeanH1 - MeanH0) / Standard_Deviation

# To get the detectable difference at different percentages, multiply the null value by 1.1, 1.2, 1.3 etc.

X <- c(1.1, 1.2, 1.3, 1.4, 1.5)

Effect_size_values <- .091 * X

print(Effect_size_values)


# Detectable difference represents the percentage exceeding the threshold that the sample size can detect.
# For example .1001 is a 10% exceedance of the threshold for 8 meals per month (.091)
# MeanH0 = the null hypothesis threshold for issuing an advisory
# 0.1001 = 10% detectable difference above the threshold
# 0.1092 = 20% detectable difference above the threshold
# 0.1183 = 30% detectable difference above the threshold
# 0.1274 = 40% detectable difference above the threshold
# 0.1365 = 50% detectable difference above the threshold



# For each percentage increase, calculate the detectable difference divided by standard deviation and feeds it into pwr.t.test()
# using standard settings:
# sig.level = 0.05 
# power = 0.80 (80% chance of detecting a real increase).

# Effect size and power analysis at a 0% detectable difference
Effect_size <- (.1 - .091) / .038

Result <- pwr.t.test(
  d = Effect_size,
  sig.level = 0.05,
  power = 0.80,
  n = NULL,
  type = "one.sample",
  alternative = "two.sided"
)


# Effect size at a 10% detectable difference

Effect_size_10 <- (0.1001 - MeanH0) / Standard_Deviation

Result <- pwr.t.test(
  d = Effect_size_10,
  sig.level = 0.05,
  power = 0.80,
  n = NULL,
  type = "one.sample",
  alternative = "two.sided"
)

N10 = 139


# Effect size at a 20% detectable difference

Effect_size_20 <- (0.1092 - MeanH0) / Standard_Deviation

Result <- pwr.t.test(
  d = Effect_size_20,
  sig.level = 0.05,
  power = 0.80,
  n = NULL,
  type = "one.sample",
  alternative = "two.sided"
)

N20 = 36

# Effect size at a 30% detectable difference

Effect_size_30 <- (0.1183 - MeanH0) / Standard_Deviation

Result <- pwr.t.test(
  d = Effect_size_30,
  sig.level = 0.05,
  power = 0.80,
  n = NULL,
  type = "one.sample",
  alternative = "two.sided"
)

N30 = 17

# Effect size at a 40% detectable difference

Effect_size_40 <- (0.1274 - MeanH0) / Standard_Deviation

Result <- pwr.t.test(
  d = Effect_size_40,
  sig.level = 0.05,
  power = 0.80,
  n = NULL,
  type = "one.sample",
  alternative = "two.sided"
)

N40 = 11

# Effect size at a 50% detectable difference

Effect_size_50 <- (0.1365 - MeanH0) / Standard_Deviation

Result <- pwr.t.test(
  d = Effect_size_50,
  sig.level = 0.05,
  power = 0.80,
  n = NULL,
  type = "one.sample",
  alternative = "two.sided"
)

N50 = 8


Samplesizes <- c(139, 36, 17, 11, 8)

Power_analysis <- as.data.frame(Samplesizes)

rownames(Power_analysis) = c(
  "10% Detectable diff",
  "20% Detectable diff",
  "30% Detectable diff",
  "40% Detectable diff",
  "50% Detectable diff"
)

print("Sample Dataframe with automatically assigned header")
Power_analysis


# One sample- two-tailed test
# d=effect size
# sig.level=significant level
# power=power of test
# type=type of test



# Display the result
# Match exact variable names
cat("Effect Size:", Effect_Size, "\n")
cat("Significance Level (alpha):", 0.05, "\n")
cat("Desired Power:", 0.80, "\n\n")
print(Result)

