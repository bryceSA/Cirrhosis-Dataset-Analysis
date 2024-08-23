# Load necessary libraries
library(readr)
library(dplyr)
library(rpart)
library(randomForest)
#install.packages("rpart.plot")
library(rpart.plot)
library(ggplot2)
library(glmnet)
#install.packages("gbm")
library(gbm)


# Load the data, remove missing values, and filter for status 'D'
dataset <- read_csv("cirrhosis.csv")


# Preprocess the data: Omit NA values, Only consider observations where status was death,
# and remove patient ID and Status from data, then Convert categorical variables to factors
data <- dataset %>%
  na.omit() %>%
  select(-c(ID, N_Days, Status)) %>%
  mutate(across(c(Drug, Sex, Ascites, Hepatomegaly, Spiders, Edema, Stage), as.factor))

#Create decision tree
#classification, control: cp = 0.02 (complexity parameter, only grow out the tree if it explains x amounto of new variation)
#minsplit (number of observations that have to be present for a split to occur)
tree <- rpart(Stage~.,method="class",control = rpart.control(cp=0.02, minsplit=1),data=data)
par(mfrow = c(1,1), xpd = NA)
plot(tree)
text(tree,use.n = T)

# Build the Random Forest model
set.seed(111)

# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(data)*0.7)
# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(data), size = data_set_size)

# Assign the data to the correct sets
training <- data[indexes,]
validation1 <- data[-indexes,]

# Perform training:
# Adjust mtry as needed, here we use sqrt(number of variables) as a starting point
rf_classifier = randomForest(Stage ~ ., data=training, ntree=9000, mtry=floor(sqrt(ncol(data)-1)), importance=TRUE)

# Output the random forest model
print(rf_classifier)
plot(rf_classifier)
rf_classifier

# Tune the model
set.seed(21)
tuneRF(data[,c(1:16)], data$Stage, stepFactor = 4)

# Variable importance plot
varImpPlot(rf_classifier) #Here we see all variables up until Hepatomegaly are less important


# Validation set assessment: looking at confusion matrix
# "Stage" is the last column in the dataset
prediction_for_table <- predict(rf_classifier, validation1[,-ncol(validation1)])
confusion_matrix <- table(Observed = validation1$Stage, Predicted = prediction_for_table)

# Calculate the accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Print the confusion matrix and accuracy
print(confusion_matrix)
print(paste0("Accuracy: ", accuracy))



#BOOSTING
set.seed(1)

# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(data)*0.7)
# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(data), size = data_set_size)

# Assign the data to the correct sets
training <- data[indexes,]
validation2 <- data[-indexes,]
# Train the boosting model
boost_model <- gbm(Stage ~ ., data=training, distribution="multinomial", 
                   n.trees=4000, interaction.depth=2, shrinkage=0.01, 
                   cv.folds=5, verbose=FALSE)

# Print variable importance
importance <- summary(boost_model, n.trees = 4000, cBars = 16)
predictions <- predict(boost_model, newdata=validation2, n.trees=9000, type="response")
predicted_classes <- colnames(predictions)[apply(predictions, 1, which.max)]

# Calculate the confusion matrix
confusion_matrix <- table(Predicted = predicted_classes, Actual = validation2$Stage)

# Print the confusion matrix
print(confusion_matrix)
# Calculate accuracy from the confusion matrix
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", accuracy))









#Part 1: EDA

# Summary statistics for numeric variables
print(summary(select(data, where(is.numeric))))

# Data distributions
numeric_vars <- select(data, where(is.numeric))
plot_list <- lapply(names(numeric_vars), function(x) {
  ggplot(data, aes_string(x = x)) +
    geom_histogram(bins = 30, fill = "blue", color = "black") +
    theme_minimal() +
    labs(title = paste("Histogram of", x), x = x, y = "Frequency")
})
print(plot_list)

# Boxplots for numeric variables
boxplot_list <- lapply(names(numeric_vars), function(x) {
  ggplot(data, aes_string(x = factor(0), y = x)) +
    geom_boxplot(fill = "cyan", color = "black") +
    theme_minimal() +
    labs(title = paste("Boxplot of", x), x = "", y = x)
})
print(boxplot_list)

# Correlation matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs")
cor_matrix

