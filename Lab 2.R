# Load the required libraries
library(caret)
library(dplyr)
library(randomForest)
library(ggplot2)

# Read the data
data <- read.csv("https://raw.githubusercontent.com/nasimm48/machine-learning/main/lab-2/data/oulad-assessments.csv")

# Remove rows where there are missing values
data <- na.omit(data)

# Convert categorical variables to factors
data <- data %>%
  mutate(across(c(code_module, code_presentation, assessment_type), as.factor))

# Binning the score variable
data$score <- cut(data$score, breaks=quantile(data$score, probs=0:3/3, na.rm=TRUE), include.lowest=TRUE, labels=c("Low", "Medium", "High"))
data$score <- as.factor(data$score)

# reproducibility
set.seed(123)  

# Split the data into training and testing sets
data_set_size <- floor(nrow(data) * 0.80)
index <- sample(1:nrow(data), size = data_set_size)

training <- data[index, ]
testing <- data[-index, ]

# Fit random forest model for classification
rf <- randomForest(score ~ code_module + code_presentation + assessment_type, data = training)

# Prediction and Result 
predictions <- predict(rf, newdata = testing, type = "response")
result <- data.frame(Actual = testing$score, Predicted = predictions)

# Generate confusion matrix
conf_matrix <- confusionMatrix(data = result$Predicted, reference = result$Actual)

# Plot the confusion matrix
confusion_plot <- ggplot(as.data.frame(conf_matrix$table), aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), color = "white", size = 3) +
  scale_fill_gradient(low = "skyblue", high = "darkblue") +
  ggtitle("Confusion Matrix") +
  theme_bw() +
  xlab("Actual Class") +
  ylab("Predicted Class") +
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, hjust = 0.5))

print(confusion_plot)
conf_matrix
