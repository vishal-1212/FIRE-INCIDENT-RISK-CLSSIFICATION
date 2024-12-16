# Load necessary libraries
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)

# Step 1: Load or simulate dataset
set.seed(123)
data <- data.frame(
  IncidentType = sample(c("Residential", "Commercial", "Industrial"), 1000, replace = TRUE),
  FireSize = runif(1000, 0.1, 10),  # Fire size in hectares
  ResponseTime = runif(1000, 1, 30),  # Response time in minutes
  NumberOfCasualties = sample(0:5, 1000, replace = TRUE),
  RiskLevel = sample(c("Low", "Medium", "High"), 1000, replace = TRUE)  # Target variable
)

# Step 2: Convert categorical variables to factors
data$IncidentType <- as.factor(data$IncidentType)
data$RiskLevel <- as.factor(data$RiskLevel)

# Step 3: Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$RiskLevel, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Step 4: Train a decision tree model
model <- rpart(RiskLevel ~ IncidentType + FireSize + ResponseTime + NumberOfCasualties,
               data = trainData,
               method = "class")

# Step 5: Visualize the decision tree
rpart.plot(model)

# Step 6: Evaluate the model on the test data
predictions <- predict(model, testData, type = "class")
confusionMatrix(predictions, testData$RiskLevel)

# Step 7: Save the trained model (optional)
saveRDS(model, "fire_risk_model.rds")

# Step 8: Load the model (if needed)
# model <- readRDS("fire_risk_model.rds")

# Step 9: Predict risk levels for new data
new_data <- data.frame(
  IncidentType = as.factor(c("Residential", "Commercial")),
  FireSize = c(2.5, 7.8),  # Fire size in hectares
  ResponseTime = c(15, 10),  # Response time in minutes
  NumberOfCasualties = c(1, 3)  # Number of casualties
)

# Ensure IncidentType matches factor levels in the training data
new_data$IncidentType <- factor(new_data$IncidentType, levels = levels(trainData$IncidentType))

# Predict risk levels for the new data
predicted_risk <- predict(model, new_data, type = "class")

# Add predictions to the new data
new_data$PredictedRisk <- predicted_risk
print(new_data)
