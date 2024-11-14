library(rpart)
library(rpart.plot)


# read data
house_loan_data <- read.csv("house_loan1.csv")

# check the data
head(house_loan_data)

# Convert categorical variables to factors
house_loan_data$job <- as.factor(house_loan_data$job)
house_loan_data$marital <- as.factor(house_loan_data$marital)
house_loan_data$education <- as.factor(house_loan_data$education)

hist(house_loan_data$age, 
     main = "Age Distribution", 
     xlab = "Age", 
     ylab = "Frequency", 
     col = "lightblue", 
     border = "black")



house_loan_data$age_group <- cut(house_loan_data$age,
                                 breaks = c(0, 25, 45, 60, Inf),
                                 labels = c("18-25", "26-45", "46-60", "60+"),
                                 right = FALSE)


table(house_loan_data$age_group)

# remove the original age column
house_loan_data <- house_loan_data[,-which(names(house_loan_data) == "age")]

# Convert target variable (housing) to 0 and 1, where "yes" is 1 and "no" is 0
house_loan_data$housing <- ifelse(house_loan_data$housing == "yes", 1, 0)
house_loan_data$housing <- as.factor(house_loan_data$housing)

# Split data into training and test sets (70% training, 30% testing)
set.seed(123)
train_indices <- sample(1:nrow(house_loan_data), size = 0.7 * nrow(house_loan_data))
train_data <- house_loan_data[train_indices, ]
test_data <- house_loan_data[-train_indices, ]

# Build a decision tree model with housing as the target variable
tree_model <- rpart(housing ~ age_group + job + marital + education, 
                    data = train_data, 
                    method = "class", 
                    control = rpart.control(cp = 0.001, maxdepth = 5))

# Visualize the decision tree
# rpart.plot(tree_model, type = 2, extra = 104, under = TRUE, fallen.leaves = TRUE, main = "Decision Tree for House Loan Prediction")
rpart.plot(tree_model, 
           type = 3,       
           extra = 101,    
           under = TRUE,    
           fallen.leaves = TRUE, 
           cex = 0.6,       
           main = "Decision Tree for House Loan Prediction")
# Predict house loan class on the test set
test_data$predicted <- predict(tree_model, newdata = test_data, type = "class")

# Generate a confusion matrix to evaluate model performance
confusion_matrix <- table(Predicted = test_data$predicted, Actual = test_data$housing)
print(confusion_matrix)


# Calculate model accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Model accuracy on the test set:", round(accuracy * 100, 2), "%"))

# Extract TN, FP, FN, TP
TN <- confusion_matrix[1, 1]
FN <- confusion_matrix[1, 2]
FP <- confusion_matrix[2, 1]
TP <- confusion_matrix[2, 2]

# Calculate Recall
recall <- TP / (TP + FN)
print(paste("Recall:", round(recall * 100, 2), "%"))

# Calculate Precision
precision <- TP / (TP + FP)
print(paste("Precision:", round(precision * 100, 2), "%"))


# ROC
library(pROC)
roc_curve <- roc(test_data$housing, as.numeric(test_data$predicted))
plot(roc_curve, main = "ROC Curve for House Loan Prediction", col = "blue")
auc <- auc(roc_curve)
print(paste("AUC:", round(auc, 2)))



feature_importance <- tree_model$variable.importance
print(feature_importance)

# Plot feature importance
barplot(feature_importance, main = "Feature Importance in Decision Tree", 
        xlab = "Features", ylab = "Importance", col = "lightblue", las = 1)

