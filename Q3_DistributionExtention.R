# Bar plot for job and housing loan
ggplot(bank_data, aes(x = job, fill = housing)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Housing Loan by Job Type", x = "Job", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("no" = "#FBB4AE", "yes" = "#B3CDE3"))  # Lighter shades

# Bar plot for marital status and housing loan
ggplot(bank_data, aes(x = marital, fill = housing)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Housing Loan by Marital Status", x = "Marital Status", y = "Proportion") +
  theme_minimal() +
  scale_fill_manual(values = c("no" = "#FBB4AE", "yes" = "#B3CDE3"))

# Bar plot for education and housing loan
ggplot(bank_data, aes(x = education, fill = housing)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Housing Loan by Education Level", x = "Education Level", y = "Proportion") +
  theme_minimal() +
  scale_fill_manual(values = c("no" = "#FBB4AE", "yes" = "#B3CDE3"))

# Box plot for age and housing loan
ggplot(bank_data, aes(x = housing, y = age, fill = housing)) +
  geom_boxplot() +
  labs(title = "Distribution of Age by Housing Loan Status", x = "Housing Loan", y = "Age") +
  theme_minimal() +
  scale_fill_manual(values = c("no" = "#FBB4AE", "yes" = "#B3CDE3"))
