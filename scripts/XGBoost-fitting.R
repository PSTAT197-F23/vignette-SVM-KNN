# Load the necessary libraries and packages

library(xgboost)
library(rsample)
library(caret)
library(dplyr)

# Load and clean our data 
load("./data/heart.rda")
heart$cp <- as.factor(heart$cp)
heart$fbs <- as.factor(heart$fbs)
heart$restecg <- as.factor(heart$restecg)
heart$exng <- as.factor(heart$exng)
heart$caa <- as.factor(heart$caa)

heart$thall <- as.factor(heart$thall)

heart$sex <- as.factor(heart$sex)
heart$slp <- as.factor(heart$slp)
# Converting features into factors is a common preprocessing step used to represent categorical variables with a fixed number of distinct values, such as gender. Factors facilitate proper handling of categorical data in statistical analyses, ensuring appropriate treatment in models. They also enhance the interpretability of visualizations by providing meaningful labels and allow the specification of order among levels for ordinal variables.
# To do this, we use the as.factor() function

# Now that we have cleaned our data, we split our dataset into training and test sets, with 80% as training and 20% as testing.
set.seed(3435)
heart_split <- initial_split(heart, strata = output, prop = 0.8)
# We use strata = output in order to perform a stratified split on the 'output' variable. This is to ensure that both the training and testing sets have a proportionate representation of each class, which can lead to more reliable model evaluation and performance assessment.
heart_train <- training(heart_split)
heart_test <- testing(heart_split)

train_x <- data.matrix(heart_train[, -14])
train_y <- heart_train[,14]

test_x = data.matrix(heart_test[, -14])
test_y = heart_test[, 14]

# Now, we convert the train and test datasets into xgb.DMatrix format.
# The conversion of training and testing data into xgb.DMatrix format in XGBoost is done for efficiency and compatibility.

xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)


# To find the optimal hyperparameters and determine the best number of boosting rounds to prevent overfitting, by performing k-fold cross-validation and hyperparameter tuning for the XGBoost model.

set.seed(3435)
# param_list is the list of parameters for our model.
param_list = list(
  # booster specifies the type of boosting model, set to 'gbtree' for gradient boosted tree.
  booster = 'gbtree',  
  # This objective indicates the purpose of this model is to perform binary classification using logistic regression.
  objective = "binary:logistic",
  # eta = 0.01 sets the learning rate to 0.01.
  eta = 0.01,
  # Gamma is the minimum loss reduction required to make a further partition on the node of the tree.
  gamma = 1,
  # max_depth = 6 indicated the maximum depth of the tree. 
  max_depth = 6,
  # subsample is the fraction of training data to randomly sample during each boosting round.
  subsample = 0.8,
  # Finally, the fraction of features to randomly sample for building each tree.
  colsample_bytree = 0.5
)

xgbcv = xgb.cv(params = param_list,
               data = xgb_train,
               nrounds = 500,
               nfold = 5,
               print_every_n = 10,
               early_stopping_rounds = 30,
               maximize = F)
# xgbcv performs cross-validated training with early stopping to find the optimal number of boosting rounds.
# k-fold strategy helps us determine the best number of rounds according to the log function to avoid overfitting. The best iteration is 413 in this case. 

# Now that we have our parameters and rounds, we can train our model.
set.seed(3435)
final.m = xgb.train(params = param_list, data = xgb_train, nrounds = 413, verbose = 0)
# Here, we see that we train the XGBoost model with nrounds = 413, as determined by our cross validation strategy.

# To determine which predictors hold the most weight in predicting our outcome in this XGBoost model, we can create a variable importance plot.
var_imp = xgb.importance(
  feature_names = setdiff(names(train),
                          c("output")),
  model = final.m)
blue_palette <- colorRampPalette(c("lightblue", "darkblue"))(length(heart) -1)
xgb.plot.importance(var_imp, col = blue_palette)
# By our VIP, we see that chest pain is the most important feature.

# Now, we can use out model to predict the outcomes of our testing data.
set.seed(3435)
predictions <- predict(final.m, newdata = test_x)
predictions <- as.numeric(predictions > 0.5)

# To measure the performance of our model, we can make a confusion matrix. 
conf_matrix <- confusionMatrix(as.factor(predictions), as.factor(test_y))
conf_matrix
# Here, we see that our model performed relatively well, with our accuracy at 0.82.

testframe <- as.data.frame(conf_matrix$table)
testframe$Prediction <- factor(testframe$Prediction, levels=rev(levels(testframe$Prediction)))

ggplot(testframe, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction")

# In this plot, we can visualize how our model performed. We see that 50 patients were classified correctly, while 11 were classified incorrectly.









