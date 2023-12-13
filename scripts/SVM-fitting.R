# Load the packages
library(dplyr)  
library(ggplot2)
library(tidyr)
library(tidyverse)
library(caret)
library(caTools)

# Import the data
dataset <- read.csv("./data/heart.csv")
head(dataset)

# Data cleaning:
#classify the variables into categorical and numerical variables 
#select the numerical variables
numeric_var <-dataset %>% 
  select("age","trtbps","chol","thalachh","oldpeak")
#select the categorical values 
categorical_var<- dataset %>%
  select("sex","cp","fbs","restecg","exng","slp","caa",
         "thall","output")%>%
  mutate_if(is.numeric, as.factor)

#combine the categorical  and numerical values
dataset1 = cbind(categorical_var,numeric_var)

head(dataset1)

# Now, we want to remove outliers.
# To do so, we need to first create a function to identify these outliers.

outliers <- function(x) { 
#IQR
Q1 <- quantile(x, probs=.25) 
Q3 <- quantile(x, probs=.75) 
iqr = Q3-Q1 

#Upper Range
upper_limit = Q3 + (iqr*1.5) 
#Lower Range Eliminating Outliers 
lower_limit = Q1 - (iqr*1.5) 

x > upper_limit | x < lower_limit 
} 
# remove the outliers
remove_outliers <- function(df_outliers, cols = names(df_outliers)) { 
  for (col in cols) { 
    df_outliers<- df_outliers[!outliers(df_outliers[[col]]),] 
  } 
  df_outliers 
}
# we have removed the outliers from the selected features 
# create new dataset without outliers
dataset2<-remove_outliers(dataset1,c("trtbps","oldpeak" ,"thalachh", "chol"))

# Next, we want to select the features, or variables, to include in our model.
set.seed(100)
#create the subsets for sizes
subsets <- c(1:8,10,13)
# define the control using random forest selection 
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   number = 10,
                   verbose = FALSE)

#run the RFE
results <- rfe(x=dataset2[, c(1:8,10:14)], y=dataset2$output,
               sizes = subsets,
               rfeControl = ctrl)

# Print the selected features
print(predictors(results))

# To visualize which predictors have the most effect on our outcomes, and therefore are most important for the model, we can make a variable importance plot

set.seed(100)
varimp_data <- data.frame(feature = row.names(varImp(results))[1:9],
                          importance = varImp(results)[1:9, 1])

ggplot(data = varimp_data, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + theme(legend.position = "none")

# Next we split our data to prepare for training and testing our model. We are using 70% of our data for training and 30% for testing

set.seed(100)
data1 <- dataset2 %>%
  select(predictors(results), "output")
head(data1)

set.seed(100)
intrain <- createDataPartition(y = data1$output, p= 0.7, list = FALSE)
training <- heart[intrain,]
testing <- heart[-intrain,]

training[["output"]] = factor(training[["output"]])

head(training)

# Now, we can construct our SVM model. 
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# We specify that the model is to be a SVM model by using method = "svmLinear" in our train function.
svm_Linear <- train(output ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10,
                    na.action = na.pass
                    )

svm_Linear

# Now that we have constructed our SVM model, we can make predictions using our model on our testing data
test_pred <- predict(svm_Linear, newdata = testing)
test_pred
testing_cleaned <- na.omit(testing)


# In order to determine how well our model performed, we can compare our actual outputs to the outputs we predicted using the SVM model.
cm = caret::confusionMatrix(table(test_pred, testing_cleaned$output))
cm$table
# From this confusion matrix table, we can see that 41 patients were correctly classified as "No", 38 patients were correctly classified as "Yes", 10 patients were incorrectly classified as 'Yes', and 10 patients were incorrectly classified as 'No'.

# We can now represent this visually through our plot:
cm <- confusionMatrix(table(test_pred, testing_cleaned$output))
plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$test_pred, levels=rev(levels(plt$test_pred)))
ggplot(plt, aes(Prediction,Var2, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("Class_1","Class_0")) +
  scale_y_discrete(labels=c("Class_0","Class_1"))

