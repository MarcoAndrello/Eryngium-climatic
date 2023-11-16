rm(list=ls())

# https://appsilon.com/r-xgboost/#modeling

library(tidyverse)
library(xgboost)
library(caTools)
library(cvms)
library(caret)

# Join demographic and environmental dataset
load("data.surv.RData")
load("xvar.RData")
left_join(data.surv, xvar, by=c("Site", "Year")) %>% filter(Year %in% c(2014:2021)) -> data
data <- na.omit(data)
data

set.seed(42)
sample_split <- sample.split(Y = data$fateSurv, SplitRatio = 0.7)
train_set <- subset(x = data, sample_split == TRUE)
test_set <- subset(x = data, sample_split == FALSE)

y_train <- as.integer(train_set$fateSurv) - 1
y_test <- as.integer(test_set$fateSurv) - 1
X_train <- train_set %>% mutate(site = as.numeric(factor(Site)), state = as.numeric(factor(State))) %>% select(site, state, ddays, num_s_Tmin_10, num_s_Tmin_15, num_s_Tmax_25, num_s_Tmax_30)
X_test <- test_set %>% mutate(site = as.numeric(factor(Site)), state = as.numeric(factor(State))) %>% select(c(site, state, ddays, num_s_Tmin_10, num_s_Tmin_15, num_s_Tmax_25, num_s_Tmax_30))

xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
xgb_test <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
xgb_params <- list(
    booster = "gbtree",
    eta = 0.01,
    max_depth = 8,
    gamma = 4,
    subsample = 0.75,
    colsample_bytree = 1,
    objective = "multi:softprob",
    eval_metric = "mlogloss",
    num_class = length(levels(data$fateSurv))
)

xgb_model <- xgb.train(
    params = xgb_params,
    data = xgb_train,
    nrounds = 5000,
    verbose = 1
)
xgb_model

importance_matrix <- xgb.importance(
    feature_names = colnames(xgb_train), 
    model = xgb_model
)
importance_matrix

xgb.plot.importance(importance_matrix)

xgb_preds <- predict(xgb_model, as.matrix(X_test), reshape = TRUE)
xgb_preds <- as.data.frame(xgb_preds)
colnames(xgb_preds) <- levels(data$fateSurv)
xgb_preds

xgb_preds$PredictedClass <- apply(xgb_preds, 1, function(y) colnames(xgb_preds)[which.max(y)])
xgb_preds$PredictedClass %>% class()
xgb_preds$ActualClass <- levels(data$fateSurv)[y_test + 1]
xgb_preds

accuracy <- sum(xgb_preds$PredictedClass == xgb_preds$ActualClass) / nrow(xgb_preds)
accuracy

confusionMatrix(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass,levels=c(0,1)))

cm <- confusionMatrix(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass,levels=c(0,1)))
cfm <- as_tibble(cm$table)
plot_confusion_matrix(cfm, target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

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