library(tidyverse)
library(caret)
library(randomForest)
library(ggplot2)
library(dplyr)

data <- read.csv("bank-full.csv")

head(data)
summary(data)

data$job <- as.factor(data$job)
data$marital <- as.factor(data$marital)
data$education <- as.factor(data$education)
data$y <- as.factor(data$y) 
data$default <- as.factor(data$default)
data$housing <- as.factor(data$housing)
data$loan <- as.factor(data$loan)
data$contact <- as.factor(data$contact)

data <- na.omit(data)


  
data$age_group <- cut(data$age, 
                      breaks = c(18, 30, 40, 50, 60, 100), 
                      labels = c("18-30", "30-40", "40-50", "50-60", "60+"), 
                      right = FALSE)


data$balance_group <- cut(data$balance, breaks = c(-Inf, 0, 1000, 5000, 10000, Inf), 
                          labels = c("Negative", "0-1000", "1000-5000", "5000-10000", "10000+"), 
                          right = FALSE)

set.seed(123)
train_index <- createDataPartition(data$y, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

rf_model <- randomForest(y ~ age + job + marital + education + default + balance + housing + loan + contact,
                         data = train_data, importance = TRUE)

importance_model = importance(rf_model)
print(importance_model)
varImpPlot(rf_model)
#Age, balance, contact, job.

rf_predictions <- predict(rf_model, test_data)

# Confusion Matrix
conf_matrix_rf <- confusionMatrix(rf_predictions, test_data$y)
print(conf_matrix_rf)

# Accuracy of Random Forest Model
rf_accuracy <- conf_matrix_rf$overall['Accuracy']
print(paste("Accuracy of Random Forest Model:", round(rf_accuracy, 2)))

# 1. Age
ggplot(data, aes(x = age_group, fill = y)) +
  geom_bar(data = subset(data, y == "yes"), alpha = 0.7, position = "dodge") +
  geom_bar(data = subset(data, y == "no"), alpha = 0.3, position = "dodge") +
  geom_text(
    stat = 'count',
    aes(label = ..count..),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  scale_fill_manual(values = c("yes" = "#1f77b4", "no" = "#d3d3d3")) +  # Dark blue for 'yes', Light gray for 'no'
  labs(title = "Age Group Distribution by Subscription Status",
       x = "Age Group", y = "Count") +
  theme_minimal()


# 2. Balance Binning
ggplot(data, aes(x = balance_group, fill = y)) +
  geom_bar(data = subset(data, y == "yes"), alpha = 0.7, position = "dodge") +
  geom_bar(data = subset(data, y == "no"), alpha = 0.3, position = "dodge") +
  geom_text(
    stat = 'count',
    aes(label = ..count..),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  scale_fill_manual(values = c("yes" = "#1f77b4", "no" = "#d3d3d3")) +  # Dark blue for 'yes', Light gray for 'no'
  labs(title = "Balance Distribution by Subscription Status (Binned)",
       x = "Balance Range", y = "Count") +
  theme_minimal()


# 3. Contact Type Distribution by Subscription Status
ggplot(data, aes(x = contact, fill = y)) +
  geom_bar(data = subset(data, y == "yes"), alpha = 0.7, position = "dodge") +
  geom_bar(data = subset(data, y == "no"), alpha = 0.3, position = "dodge") +
  geom_text(
    stat = 'count',
    aes(label = ..count..),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  scale_fill_manual(values = c("yes" = "#1f77b4", "no" = "#d3d3d3")) +  # Dark blue for 'yes', Light gray for 'no'
  labs(title = "Contact Type by Subscription Status",
       x = "Contact Type", y = "Count") +
  theme_minimal()


# 4. Job Type Distribution by Subscription Status
ggplot(data, aes(x = job, fill = y)) +
  geom_bar(data = subset(data, y == "yes"), alpha = 0.7, position = "dodge") +
  geom_bar(data = subset(data, y == "no"), alpha = 0.3, position = "dodge") +
  geom_text(
    stat = 'count',
    aes(label = ..count..),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  scale_fill_manual(values = c("yes" = "#1f77b4", "no" = "#d3d3d3")) +  # Dark blue for 'yes', Light gray for 'no'
  labs(title = "Job Type Distribution by Subscription Status",
       x = "Job Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




age_summary <- data %>%
  group_by(age_group, y) %>%
  summarise(count = n()) %>%
  group_by(y) %>%  
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(y, desc(percentage))


print(age_summary)
# Pie chart for 'No' Subscription with percentages
no_data <- age_summary %>% filter(y == "no")
no_data <- no_data %>% mutate(percentage_label = paste0(round(percentage, 1), "%"))

ggplot(no_data, aes(x = "", y = count, fill = age_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("30-40" = "#1f77b4", "40-50" = "#ff7f0e", 
                               "50-60" = "#2ca02c", "18-30" = "#d62728", 
                               "60+" = "#9467bd")) +
  geom_text(aes(label = percentage_label), position = position_stack(vjust = 0.5)) +
  labs(title = "Age Group Distribution for 'No' Subscription") +
  theme_void()

# Pie chart for 'Yes' Subscription with percentages
yes_data <- age_summary %>% filter(y == "yes")
yes_data <- yes_data %>% mutate(percentage_label = paste0(round(percentage, 1), "%"))

ggplot(yes_data, aes(x = "", y = count, fill = age_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("30-40" = "#1f77b4", "40-50" = "#ff7f0e", 
                               "50-60" = "#2ca02c", "18-30" = "#d62728", 
                               "60+" = "#9467bd")) +
  geom_text(aes(label = percentage_label), position = position_stack(vjust = 0.5)) +
  labs(title = "Age Group Distribution for 'Yes' Subscription") +
  theme_void()





balance_summary <- data %>%
  group_by(balance_group, y) %>%
  summarise(count = n()) %>%
  group_by(y) %>%  
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(y, desc(percentage))

print(balance_summary)
# Pie chart for 'No' Subscription with percentages (Balance Groups)
no_balance_data <- balance_summary %>% filter(y == "no")
no_balance_data <- no_balance_data %>% mutate(percentage_label = paste0(round(percentage, 1), "%"))

ggplot(no_balance_data, aes(x = "", y = count, fill = balance_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("0-1000" = "#1f77b4", "1000-5000" = "#ff7f0e", 
                               "Negative" = "#2ca02c", "5000-10000" = "#d62728", 
                               "10000+" = "#9467bd")) +
  geom_text(aes(label = percentage_label), position = position_stack(vjust = 0.5)) +
  labs(title = "Balance Group Distribution for 'No' Subscription") +
  theme_void()

# Pie chart for 'Yes' Subscription with percentages (Balance Groups)
yes_balance_data <- balance_summary %>% filter(y == "yes")
yes_balance_data <- yes_balance_data %>% mutate(percentage_label = paste0(round(percentage, 1), "%"))

ggplot(yes_balance_data, aes(x = "", y = count, fill = balance_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("0-1000" = "#1f77b4", "1000-5000" = "#ff7f0e", 
                               "Negative" = "#2ca02c", "5000-10000" = "#d62728", 
                               "10000+" = "#9467bd")) +
  geom_text(aes(label = percentage_label), position = position_stack(vjust = 0.5)) +
  labs(title = "Balance Group Distribution for 'Yes' Subscription") +
  theme_void()




contact_summary <- data %>%
  group_by(contact, y) %>%
  summarise(count = n()) %>%
  group_by(y) %>% 
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(y, desc(percentage))

print(contact_summary)

# Pie chart for 'No' Subscription with percentages (Contact Types)
no_contact_data <- contact_summary %>% filter(y == "no")
no_contact_data <- no_contact_data %>% mutate(percentage_label = paste0(round(percentage, 1), "%"))

ggplot(no_contact_data, aes(x = "", y = count, fill = contact)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("cellular" = "#1f77b4", "unknown" = "#ff7f0e", "telephone" = "#2ca02c")) +
  geom_text(aes(label = percentage_label), position = position_stack(vjust = 0.5)) +
  labs(title = "Contact Type Distribution for 'No' Subscription") +
  theme_void()

# Pie chart for 'Yes' Subscription with percentages (Contact Types)
yes_contact_data <- contact_summary %>% filter(y == "yes")
yes_contact_data <- yes_contact_data %>% mutate(percentage_label = paste0(round(percentage, 1), "%"))

ggplot(yes_contact_data, aes(x = "", y = count, fill = contact)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("cellular" = "#1f77b4", "unknown" = "#ff7f0e", "telephone" = "#2ca02c")) +
  geom_text(aes(label = percentage_label), position = position_stack(vjust = 0.5)) +
  labs(title = "Contact Type Distribution for 'Yes' Subscription") +
  theme_void()


job_summary <- data %>%
  group_by(job, y) %>%
  summarise(count = n()) %>%
  group_by(y) %>% 
  mutate(total_count = sum(count),
         percentage = count / total_count * 100) %>%
  arrange(y, desc(percentage))

print(job_summary)

# Bar plot for Job Distribution for 'No' Subscription with percentage labels
ggplot(job_summary %>% filter(y == "no"), aes(x = job, y = percentage, fill = y)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +  # Add percentage labels
  labs(title = "Job Distribution for 'No' Subscription",
       x = "Job Type", y = "Percentage") +
  scale_fill_manual(values = c("no" = "#d3d3d3")) +  # Custom color for 'No'
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +  # Rotate and reduce font size
  theme_minimal()

# Bar plot for Job Distribution for 'Yes' Subscription with percentage labels
ggplot(job_summary %>% filter(y == "yes"), aes(x = job, y = percentage, fill = y)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +  # Add percentage labels
  labs(title = "Job Distribution for 'Yes' Subscription",
       x = "Job Type", y = "Percentage") +
  scale_fill_manual(values = c("yes" = "#1f77b4")) +  # Custom color for 'Yes'
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +  # Rotate and reduce font size
  theme_minimal()





