rm(list=ls())

# https://appsilon.com/r-xgboost/#modeling

library(tidyverse)
library(xgboost)
library(caTools)
library(cvms)
library(caret)
library(Matrix)
library(data.table)

# # Join demographic and environmental dataset
# ## Survival
# load("data.surv.RData")
# load("xvar.RData")
# left_join(data.surv, xvar, by=c("Site", "Year")) %>% filter(Year %in% c(2014:2021)) -> data
# data <- na.omit(data)
# data %>% mutate(fateBin = fateSurv) %>% select(-fateSurv) -> data
# data

## Flowering
load("data.flow.RData")
load("xvar.RData")
left_join(data.flow, xvar, by=c("Site", "Year")) %>% filter(Year %in% c(2014:2021)) -> data
data <- na.omit(data)
data %>% mutate(fateBin = fateFlow) %>% select(-fateFlow) -> data
data

# Code Site and State as categorical variables using one-hot encoding
data %>% select(-c(Quad, num_2005, ID, Year, Fate)) -> data # Remove unnecessary columns
data_sparse <- sparse.model.matrix(fateBin ~., data=data)[,-1]

# Split sample in a train and a test sumbsamples
set.seed(24)
sample_split <- sample.split(Y = data$fateBin, SplitRatio = 0.7)
X_train <- subset(x = as.matrix(data_sparse), sample_split == TRUE)
X_test <- subset(x = as.matrix(data_sparse), sample_split == FALSE)
y_train <- as.integer(data$fateBin[sample_split== TRUE]) - 1
y_test <- as.integer(data$fateBin[sample_split== FALSE]) - 1

# Construct xgb.DMatrix
xgb_train <- xgb.DMatrix(data = X_train, label = y_train)
xgb_test <- xgb.DMatrix(data = X_test, label = y_test)

# Set xgb parameters
xgb_params <- list(
    booster = "gbtree",
    eta = 0.01,
    max_depth = 8,
    gamma = 4,
    subsample = 0.75,
    colsample_bytree = 1,
    objective = "multi:softprob",
    eval_metric = "mlogloss",
    num_class = length(levels(data$fateBin))
)

# Train the xgb model
xgb_model <- xgb.train(
    params = xgb_params,
    data = xgb_train,
    nrounds = 5000,
    verbose = 1
)
xgb_model

# Variable importance
importance_matrix <- xgb.importance(
    feature_names = colnames(xgb_train), 
    model = xgb_model
)
importance_matrix
xgb.plot.importance(importance_matrix)

# Predict on the test subsample
xgb_preds <- predict(xgb_model, as.matrix(X_test), reshape = TRUE)
xgb_preds <- predict(xgb_model, X_test, reshape = TRUE)
xgb_preds <- as.data.frame(xgb_preds)
colnames(xgb_preds) <- levels(data$fateBin)
xgb_preds
# Add column saying which fate is predicted by the xgb model
xgb_preds$PredictedClass <- apply(xgb_preds, 1, function(y) colnames(xgb_preds)[which.max(y)])
# Add column saying which fate was observed in the data
xgb_preds$ActualClass <- levels(data$fateBin)[y_test + 1]
xgb_preds
# Accuracy: number of correct predictions / number of obs, in the test set
accuracy <- sum(xgb_preds$PredictedClass == xgb_preds$ActualClass) / nrow(xgb_preds)
accuracy

# Congfusion matrix
cm <- confusionMatrix(data = factor(xgb_preds$PredictedClass,levels=c(0,1)), reference=factor(xgb_preds$ActualClass))
cfm <- as_tibble(cm$table)
plot_confusion_matrix(cfm, target_col = "Reference", prediction_col = "Prediction", counts_col = "n")
cm

# Emerge che le variabili importanti per la sopravvivenza sono ddays, num_sd_Tmin_10, num_sp_Tmin_10, State e Site
# Emerge che le variabili importanti per la fioritura sono ddays, num_sd_Tmin_10, num_sp_Tmin_10, State e Site e num_sd/sp_Tmax_25

# POI PROVARE STRATIFICATION ON MULTIPLE COLUMNS
# https://cran.r-project.org/web/packages/splitTools/vignettes/splitTools.html


# a <- logistic_reg(engine="glm") %>% 
#     fit(fateSurv ~ Site*State*(ddays + num_s_Tmin_15), data=data)
# summary(a)
# 
# b <- boost_tree(mode="classification") %>% 
#     fit(fateSurv ~ Site*State*(ddays + num_s_Tmin_15), data=data)
# tidy(b)
# summary(b$fit)
# library(xgboost)
# xgb.importance(b$fit)
# 
# ## Classification:
# ##data(iris)
# set.seed(71)
# iris.rf <- randomForest(Species ~ ., data=iris, importance=TRUE,
#                         proximity=TRUE)
# print(iris.rf)
# ## Look at variable importance:
# round(importance(iris.rf), 2)
# ## The `unsupervised' case:
# set.seed(17)
# iris.urf <- randomForest(iris[, -5])
# MDSplot(iris.urf, iris$Species)
# ##
# 
# 
# show_engines("linear_reg")
# linear_reg()
# a <- linear_reg(mode = "regression", engine = "lm", penalty = NULL, mixture = NULL)