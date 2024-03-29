# Data Import -------------------------------------------------------------
# Set working directory (modify file path to location of dataset)
setwd("~/Documents/ACTL4001/SOA/Materials")

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)

# Read the dataset
inforce <- read.csv("2024-srcsc-superlife-inforce-dataset.csv", skip=3)

# View the structure of the dataset
str(inforce)

# Calculating Total Payouts -----------------------------------------------
# Define subsets for each segment
M_S_high <- subset(inforce, Sex == 'M' & Smoker.Status == "S" & Underwriting.Class =="high risk")
M_S_moderate <- subset(inforce, Sex == 'M' & Smoker.Status == "S" & Underwriting.Class =="moderate risk")
M_S_low <- subset(inforce, Sex == 'M' & Smoker.Status == "S" & Underwriting.Class =="low risk")
M_S_verylow <- subset(inforce, Sex == 'M' & Smoker.Status == "S" & Underwriting.Class =="very low risk")
M_NS_high <- subset(inforce, Sex == 'M' & Smoker.Status == "NS" & Underwriting.Class =="high risk")
M_NS_moderate <- subset(inforce, Sex == 'M' & Smoker.Status == "NS" & Underwriting.Class =="moderate risk")
M_NS_low <- subset(inforce, Sex == 'M' & Smoker.Status == "NS" & Underwriting.Class =="low risk")
M_NS_verylow <- subset(inforce, Sex == 'M' & Smoker.Status == "NS" & Underwriting.Class =="very low risk")
F_S_high <- subset(inforce, Sex == 'F' & Smoker.Status == "S" & Underwriting.Class =="high risk")
F_S_moderate <- subset(inforce, Sex == 'F' & Smoker.Status == "S" & Underwriting.Class =="moderate risk")
F_S_low <- subset(inforce, Sex == 'F' & Smoker.Status == "S" & Underwriting.Class =="low risk")
F_S_verylow <- subset(inforce, Sex == 'F' & Smoker.Status == "S" & Underwriting.Class =="very low risk")
F_NS_high <- subset(inforce, Sex == 'F' & Smoker.Status == "NS" & Underwriting.Class =="high risk")
F_NS_moderate <- subset(inforce, Sex == 'F' & Smoker.Status == "NS" & Underwriting.Class =="moderate risk")
F_NS_low <- subset(inforce, Sex == 'F' & Smoker.Status == "NS" & Underwriting.Class =="low risk")
F_NS_verylow <- subset(inforce, Sex == 'F' & Smoker.Status == "NS" & Underwriting.Class =="very low risk")

# Function to calculate total payout
calculate_total_payout <- function(data) {
  total_payout <- data %>%
    filter(Death.indicator == 1 & Year.of.Death > 2003) %>%
    group_by(Year.of.Death) %>%
    summarise(Total_Payout = sum(Face.amount))
  return(total_payout)
}

# Calculate total payout for each segment
M_S_high_payout <- calculate_total_payout(M_S_high)
M_S_moderate_payout <- calculate_total_payout(M_S_moderate)
M_S_low_payout <- calculate_total_payout(M_S_low)
M_S_verylow_payout <- calculate_total_payout(M_S_verylow)
M_NS_high_payout <- calculate_total_payout(M_NS_high)
M_NS_moderate_payout <- calculate_total_payout(M_NS_moderate)
M_NS_low_payout <- calculate_total_payout(M_NS_low)
M_NS_verylow_payout <- calculate_total_payout(M_NS_verylow)
F_S_high_payout <- calculate_total_payout(F_S_high)
F_S_moderate_payout <- calculate_total_payout(F_S_moderate)
F_S_low_payout <- calculate_total_payout(F_S_low)
F_S_verylow_payout <- calculate_total_payout(F_S_verylow)
F_NS_high_payout <- calculate_total_payout(F_NS_high)
F_NS_moderate_payout <- calculate_total_payout(F_NS_moderate)
F_NS_low_payout <- calculate_total_payout(F_NS_low)
F_NS_verylow_payout <- calculate_total_payout(F_NS_verylow)

# Create a new workbook
wb <- createWorkbook()

# Add sheets and write data for each segment
addWorksheet(wb, "M_S_high")
writeData(wb, "M_S_high", M_S_high_payout)
addWorksheet(wb, "M_S_moderate")
writeData(wb, "M_S_moderate", M_S_moderate_payout)
addWorksheet(wb, "M_S_low")
writeData(wb, "M_S_low", M_S_low_payout)
addWorksheet(wb, "M_S_verylow")
writeData(wb, "M_S_verylow", M_S_verylow_payout)
addWorksheet(wb, "M_NS_high")
writeData(wb, "M_NS_high", M_NS_high_payout)
addWorksheet(wb, "M_NS_moderate")
writeData(wb, "M_NS_moderate", M_NS_moderate_payout)
addWorksheet(wb, "M_NS_low")
writeData(wb, "M_NS_low", M_NS_low_payout)
addWorksheet(wb, "M_NS_verylow")
writeData(wb, "M_NS_verylow", M_NS_verylow_payout)
addWorksheet(wb, "F_S_high")
writeData(wb, "F_S_high", F_S_high_payout)
addWorksheet(wb, "F_S_moderate")
writeData(wb, "F_S_moderate", F_S_moderate_payout)
addWorksheet(wb, "F_S_low")
writeData(wb, "F_S_low", F_S_low_payout)
addWorksheet(wb, "F_S_verylow")
writeData(wb, "F_S_verylow", F_S_verylow_payout)
addWorksheet(wb, "F_NS_high")
writeData(wb, "F_NS_high", F_NS_high_payout)
addWorksheet(wb, "F_NS_moderate")
writeData(wb, "F_NS_moderate", F_NS_moderate_payout)
addWorksheet(wb, "F_NS_low")
writeData(wb, "F_NS_low", F_NS_low_payout)
addWorksheet(wb, "F_NS_verylow")
writeData(wb, "F_NS_verylow", F_NS_verylow_payout)

# Save the workbook to a file
saveWorkbook(wb, "PayoutTables.xlsx", overwrite = TRUE)

# Notify the user
cat("Payout tables exported to PayoutTables.xlsx successfully!")
