# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset
# Replace 'bank_data.csv' with the actual path to your CSV file
bank_data <- read.csv("house_loan1.csv")

# Inspect the data to understand its structure
str(bank_data)
summary(bank_data)

# Convert necessary variables to factors (if not already done)
bank_data$housing <- factor(bank_data$housing, levels = c("no", "yes"))
bank_data$job <- as.factor(bank_data$job)
bank_data$marital <- as.factor(bank_data$marital)
bank_data$education <- as.factor(bank_data$education)

### 1. Chi-Square Tests for Categorical Variables

# Chi-Square test for association between job and housing loan
job_table <- table(bank_data$job, bank_data$housing)
chi_square_job <- chisq.test(job_table)
print("Chi-Square Test for Job and Housing Loan")
print(chi_square_job)

# Chi-Square test for marital status and housing loan
marital_table <- table(bank_data$marital, bank_data$housing)
chi_square_marital <- chisq.test(marital_table)
print("Chi-Square Test for Marital Status and Housing Loan")
print(chi_square_marital)

# Chi-Square test for education and housing loan
education_table <- table(bank_data$education, bank_data$housing)
chi_square_education <- chisq.test(education_table)
print("Chi-Square Test for Education and Housing Loan")
print(chi_square_education)

### 2. T-Test/ANOVA for Age and Housing Loan

# T-test to see if there's a significant difference in age based on housing loan status
t_test_age <- t.test(age ~ housing, data = bank_data)
print("T-Test for Age and Housing Loan")
print(t_test_age)

# Alternatively, use ANOVA if you'd like to expand this analysis with more groups later
anova_age <- aov(age ~ housing, data = bank_data)
print("ANOVA for Age and Housing Loan")
summary(anova_age)

### 3. Visualizations

# Bar plot for job and housing loan
ggplot(bank_data, aes(x = job, fill = housing)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Housing Loan by Job Type", x = "Job", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar plot for marital status and housing loan
ggplot(bank_data, aes(x = marital, fill = housing)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Housing Loan by Marital Status", x = "Marital Status", y = "Proportion") +
  theme_minimal()

# Bar plot for education and housing loan
ggplot(bank_data, aes(x = education, fill = housing)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Housing Loan by Education Level", x = "Education Level", y = "Proportion") +
  theme_minimal()

# Box plot for age and housing loan
ggplot(bank_data, aes(x = housing, y = age, fill = housing)) +
  geom_boxplot() +
  labs(title = "Distribution of Age by Housing Loan Status", x = "Housing Loan", y = "Age") +
  theme_minimal()
