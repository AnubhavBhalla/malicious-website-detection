
# Malicious Website Detection in R using Random Forest, SVM, and KNN

# Load required packages
library(caret)
library(randomForest)
library(e1071)  # for SVM
library(class)  # for KNN

# Set seed for reproducibility
set.seed(123)

# Load dataset
# Replace this with actual path if dataset is in CSV format
# dataset <- read.csv("Malicious_and_benign_websites1.csv")

# Sample dataset for demonstration (simulate with 1000 rows and 10 features)
n <- 1000
dataset <- data.frame(
  URL_LENGTH = runif(n, 10, 200),
  NUMBER_SPECIAL_CHARACTERS = runif(n, 0, 20),
  SERVER = factor(sample(c("nginx", "apache", "IIS"), n, replace = TRUE)),
  WHOIS_REGDATE = sample(2000:2024, n, replace = TRUE),
  TCP_CONVERSATION_EXCHANGE = runif(n, 50, 500),
  label = factor(sample(c("malicious", "benign"), n, replace = TRUE))
)

# Preprocessing
dataset$SERVER <- as.numeric(factor(dataset$SERVER))  # Convert factor to numeric
dataset$WHOIS_REGDATE <- scale(dataset$WHOIS_REGDATE)
dataset$URL_LENGTH <- scale(dataset$URL_LENGTH)
dataset$NUMBER_SPECIAL_CHARACTERS <- scale(dataset$NUMBER_SPECIAL_CHARACTERS)
dataset$TCP_CONVERSATION_EXCHANGE <- scale(dataset$TCP_CONVERSATION_EXCHANGE)

# Train-test split (70/30)
index <- createDataPartition(dataset$label, p = 0.7, list = FALSE)
train_data <- dataset[index, ]
test_data <- dataset[-index, ]

# Train Random Forest model
rf_model <- train(label ~ ., data = train_data, method = "rf", trControl = trainControl(method = "cv", number = 10))
rf_pred <- predict(rf_model, test_data)
rf_conf <- confusionMatrix(rf_pred, test_data$label)

# Train SVM model
svm_model <- train(label ~ ., data = train_data, method = "svmRadial", trControl = trainControl(method = "cv", number = 10))
svm_pred <- predict(svm_model, test_data)
svm_conf <- confusionMatrix(svm_pred, test_data$label)

# Train KNN model
knn_model <- train(label ~ ., data = train_data, method = "knn", trControl = trainControl(method = "cv", number = 10))
knn_pred <- predict(knn_model, test_data)
knn_conf <- confusionMatrix(knn_pred, test_data$label)

# Print Results
print("Random Forest Performance:")
print(rf_conf)

print("SVM Performance:")
print(svm_conf)

print("KNN Performance:")
print(knn_conf)
