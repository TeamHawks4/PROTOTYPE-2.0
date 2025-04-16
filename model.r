# Load libraries
library(tidyverse)
library(neuralnet)
library(caret)

# Step 1: Load data
data <- read.csv("smart_city_citizen_activity (1).csv")

# Step 2: Feature selection
data <- data %>%
  select(-Citizen_ID, -Sleep_Hours, -Public_Events_Hours,
         -Calories_Burned, -Social_Media_Hours, -Work_Hours)

# Step 3: Encode categorical variables
data$Gender <- as.factor(data$Gender)
data$Mode_of_Transport <- as.factor(data$Mode_of_Transport)

# One-hot encoding
dummies <- dummyVars(~ ., data = data)
data_processed <- as.data.frame(predict(dummies, newdata = data))

# Step 4: Normalize data
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
data_scaled <- as.data.frame(lapply(data_processed, normalize))

# Step 5: Train-test split
set.seed(123)
index <- createDataPartition(data_scaled$Carbon_Footprint_kgCO2, p = 0.8, list = FALSE)
train_data <- data_scaled[index, ]
test_data <- data_scaled[-index, ]

# Step 6: Create formula
target <- "Carbon_Footprint_kgCO2"
features <- setdiff(names(train_data), target)
formula <- as.formula(paste(target, "~", paste(features, collapse = " + ")))

# Step 7: Train neural network with 2 hidden layers (8 and 4 neurons)
set.seed(123)
nn_model <- neuralnet(formula,
                      data = train_data,
                      hidden = c(8, 4),
                      linear.output = TRUE,
                      stepmax = 1e6,
                      lifesign = "minimal")

# Step 8: Predict and evaluate
predicted <- compute(nn_model, test_data[, features])$net.result
actual <- test_data$Carbon_Footprint_kgCO2

# Calculate RMSE and R-squared
rmse <- sqrt(mean((predicted - actual)^2))
r2 <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)

cat("RMSE:", round(rmse, 4), "\n")
cat("R-squared:", round(r2, 4), "\n")
plot(nn_model)