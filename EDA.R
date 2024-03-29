# Data Import -------------------------------------------------------------
# Set working directory (modify file path to location of dataset)
setwd("~/Documents/ACTL4001/SOA/Materials")

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Read the dataset
inforce <- read.csv("2024-srcsc-superlife-inforce-dataset.csv", skip=3)

# View the structure of the dataset
str(inforce)

# Data Cleaning -----------------------------------------------------------
# Rename variables for consistency
inforce <- rename(inforce, 
                  Smoker.status = Smoker.Status, 
                  Underwriting.class = Underwriting.Class, 
                  Urban.vs.rural = Urban.vs.Rural, 
                  Distribution.channel = Distribution.Channel, 
                  Year.of.death = Year.of.Death, 
                  Lapse.indicator = Lapse.Indicator,
                  Year.of.lapse = Year.of.Lapse, 
                  Cause.of.death = Cause.of.Death)

# Convert categorical variables to factors
inforce$Policy.type <- as.factor(inforce$Policy.type)
inforce$Sex <- as.factor(inforce$Sex)
inforce$Smoker.status <- as.factor(inforce$Smoker.status)
inforce$Underwriting.class <- as.factor(inforce$Underwriting.class)
inforce$Urban.vs.rural <- as.factor(inforce$Urban.vs.rural)
inforce$Region <- as.factor(inforce$Region)
inforce$Distribution.channel <- as.factor(inforce$Distribution.channel)

# Convert Death.indicator and Lapse.indicator to logical
inforce <- inforce %>%
  mutate(Death.indicator = replace(Death.indicator,is.na(Death.indicator),0))
inforce$Death.indicator <- as.logical(inforce$Death.indicator)
inforce <- inforce %>%
  mutate(Lapse.indicator = replace(Lapse.indicator,is.na(Lapse.indicator),0)) %>%
  mutate(Lapse.indicator = replace(Lapse.indicator,Lapse.indicator == "Y",1))
inforce$Lapse.indicator <- as.logical(as.numeric(inforce$Lapse.indicator))

# Data Analysis -----------------------------------------------------------
# Summary statistics
summary(inforce)

# Distribution of Policy types
table(inforce$Policy.type)

# Distribution of Sex
table(inforce$Sex)

# Distribution of Smoker status
table(inforce$Smoker.status)

# Visualize the distribution of Face amount
ggplot(inforce, aes(x = Face.amount)) +
  geom_histogram(binwidth = 50000, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Face Amount",
       x = "Face Amount",
       y = "Frequency")

# Analyze death rates by Policy type
death_rates <- inforce %>%
  group_by(Policy.type) %>%
  summarize(Death_rate = mean(Death.indicator, na.rm = TRUE))

# Visualize death rates by Policy type
ggplot(death_rates, aes(x = Policy.type, y = Death_rate)) +
  geom_bar(stat = "identity", fill = "red", color = "black") +
  labs(title = "Death Rates by Policy Type",
       x = "Policy Type",
       y = "Death Rate")

# Distribution of Issue years
ggplot(inforce, aes(x = Issue.year)) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Distribution of Policy Issue Years",
       x = "Issue Year",
       y = "Frequency")

# Distribution of Issue ages
ggplot(inforce, aes(x = Issue.age)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Issue Ages",
       x = "Issue Age",
       y = "Frequency")

# Analyze death rates by Sex
death_rates_sex <- inforce %>%
  group_by(Sex) %>%
  summarize(Death_rate = mean(Death.indicator, na.rm = TRUE))

# Visualize death rates by Sex
ggplot(death_rates_sex, aes(x = Sex, y = Death_rate)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Death Rates by Sex",
       x = "Sex",
       y = "Death Rate")

# Analyze death rates by Smoker status
death_rates_smoker <- inforce %>%
  group_by(Smoker.status) %>%
  summarize(Death_rate = mean(Death.indicator, na.rm = TRUE))

# Visualize death rates by Smoker status
ggplot(death_rates_smoker, aes(x = Smoker.status, y = Death_rate)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  labs(title = "Death Rates by Smoker Status",
       x = "Smoker Status",
       y = "Death Rate")

# Analyze death rates by Underwriting class
death_rates_underwriting <- inforce %>%
  group_by(Underwriting.class) %>%
  summarize(Death_rate = mean(Death.indicator, na.rm = TRUE))

# Visualize death rates by Underwriting class
ggplot(death_rates_underwriting, aes(x = Underwriting.class, y = Death_rate)) +
  geom_bar(stat = "identity", fill = "yellow", color = "black") +
  labs(title = "Death Rates by Underwriting Class",
       x = "Underwriting Class",
       y = "Death Rate")

# Analyze death rates by Urban vs Rural
death_rates_urban_rural <- inforce %>%
  group_by(Urban.vs.rural) %>%
  summarize(Death_rate = mean(Death.indicator, na.rm = TRUE))

# Visualize death rates by Urban vs Rural
ggplot(death_rates_urban_rural, aes(x = Urban.vs.rural, y = Death_rate)) +
  geom_bar(stat = "identity", fill = "pink", color = "black") +
  labs(title = "Death Rates by Urban vs Rural",
       x = "Urban vs Rural",
       y = "Death Rate")


# Data Quality Checks -----------------------------------------------------
# Check for missing values
missing_values <- colSums(is.na(inforce))
print("Missing Values:")
print(missing_values)

# Treat missing values in Cause.of.death
inforce$Cause.of.death[is.na(inforce$Cause.of.death)] <- "Unknown"

# Check for duplicate rows
duplicate_rows <- inforce[duplicated(inforce), ]
print("Duplicate Rows:")
print(duplicate_rows)

# Check for outliers in numeric variables
numeric_vars <- sapply(inforce, is.numeric)
numeric_data <- inforce[, numeric_vars]
outliers <- sapply(numeric_data, function(x) {
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  outliers <- x < (qnt[1] - H) | x > (qnt[2] + H)
  sum(outliers, na.rm = TRUE)
})
print("Outliers:")
print(outliers)

# Create boxplots for Year.of.death and Year.of.lapse
ggplot(inforce, aes(x = "", y = Year.of.death)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "Boxplot of Year of Death",
       x = NULL,
       y = "Year of Death")

ggplot(inforce, aes(x = "", y = Year.of.lapse)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(title = "Boxplot of Year of Lapse",
       x = NULL,
       y = "Year of Lapse")

# Check for unusual values in categorical variables
categorical_vars <- sapply(inforce, is.factor)
categorical_data <- inforce[, categorical_vars]
unusual_values <- lapply(categorical_data, function(x) unique(x[!is.na(x)]))
print("Unusual Values:")
print(unusual_values)


# Data Export -------------------------------------------------------------
# Export the cleaned dataset to a CSV file
write.csv(inforce, "cleaned_inforce_dataset.csv", row.names = FALSE)

# View the structure of the dataset
str(inforce)



