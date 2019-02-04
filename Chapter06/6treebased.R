library(magrittr)
install.packages("Boruta")
install.packages("caret")
install.packages("classifierplots")
install.packages("InformationValue")
install.packages("MLmetrics")
install.packages("randomForest")
install.packages("ROCR")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("tidyverse")
install.packages("xgboost")
options(scipen=999)

# RPART -------------------------------------------------------------------
sim_df <- read.csv("C:/Users/cory/Desktop/data/sim_df.csv", stringsAsFactors = FALSE)

table(sim_df$y)
sim_df$y <- as.factor(sim_df$y)

set.seed(1066)
index <- caret::createDataPartition(sim_df$y, p = 0.7, list = F)
train <- sim_df[index, ]
test <- sim_df[-index, ]

tree_fit <- rpart::rpart(y ~ ., data = train)
                         
tree_fit$cptable

rpart::plotcp(tree_fit)

cp = tree_fit$cptable[4, 1]
cp
# not run
# rpart::prune(tree_fit, cp = cp)
# Or
# rpart::prune(tree_fit, cp = 0.01219512)

rpart.plot::rpart.plot(
  tree_fit,
  type = 3,
  extra = 2,
  branch = .75,
  under = TRUE
) 

rpart.plot::rpart.plot(
  tree_fit,
  type = 1,
  extra = 6,
  branch = .75,
  under = TRUE
)

rpart.plot::rpart.rules(tree_fit)

rparty.test <- predict(tree_fit, newdata = test)

rparty.test <- rparty.test[, 2]

classifierplots::density_plot(test$y, rparty.test)

ynum <- as.numeric(ifelse(test$y == "1", 1, 0))

MLmetrics::AUC(rparty.test, ynum) #.82

MLmetrics::LogLoss(rparty.test, ynum) #.41

rp <- predict(tree_fit, test, type = "class")
table(test$y, rp)
# santander ---------------------------------------------------------------

# saveRDS(santander_prepd, "santander_prepd.RData")

santander <- readRDS("santander_prepd.RData")

table(santander$y)

# set same seed as before

set.seed(1966)

trainIndex <- caret::createDataPartition(santander$y, p = 0.8, list = FALSE)
train <- santander[trainIndex, ]
test <- santander[-trainIndex, ]

train_zero <- caret::nearZeroVar(train, saveMetrics = TRUE)
table(train_zero$zeroVar)
train <- train[, train_zero$zeroVar == 'FALSE']

x <- as.matrix(train[, -142])
y <- as.factor(train$y)

set.seed(1999)
forest_fit <- randomForest::randomForest(x = x, y = y,
                                         ntree = 200,
                                         sampsize = c(3600, 1200))
forest_fit
# plot(forest_fit)

which.min(forest_fit$err.rate[, 1]) 
forest_fit$err.rate[105]

randomForest::varImpPlot(forest_fit)

ff <- data.frame(unlist(forest_fit$importance))

ff$var <- row.names(ff)

summary(ff)

my_forest_vars <- dplyr::filter(ff, MeanDecreaseGini > 4.02)

my_forest_vars <- my_forest_vars$var

x_reduced <- x[, my_forest_vars]
dim(x_reduced)

set.seed(567)
forest_fit2 <- randomForest::randomForest(x = x_reduced, y = y,
                                          ntree = 110, 
                                          sampsize = c(3600, 1200)) 

which.min(forest_fit2$err.rate[, 1]) 

randomForest::varImpPlot(forest_fit2)

# rf_pred <- predict(forest_fit)

rf_prob <- predict(forest_fit, type = "prob")

y_prob <- rf_prob[, 2]

classifierplots::density_plot(y, y_prob)

ynum <- as.numeric(ifelse(y == "1", 1, 0))
MLmetrics::AUC(y_prob, ynum) #.815
MLmetrics::LogLoss(y_prob, ynum) #.265

# correction function

corrected_prob <- function(result, population_fraction, sample_fraction){
  value <- 1/(1+(1/population_fraction-1) / (1/sample_fraction-1)*(1/result-1))
  return(value)
}

# input <- c(0.1, 0.2, 0.3)
# corrected_probability(result = p_1,
#                       population_fraction = 0.05,
#                       sample_fraction = 0.5)

yprob_corrected <- corrected_prob(result = y_prob,
                                        population_fraction = 0.04,
                                        sample_fraction = .33)

MLmetrics::AUC(yprob_corrected, ynum) #.81
MLmetrics::LogLoss(yprob_corrected, ynum)
# classifierplots::density_plot(y, yprob_corrected)

# InformationValue::optimalCutoff(ynum, yprob_corrected, optimiseFor = "Both") #0.0238

rf_test <- predict(forest_fit, type = "prob", newdata = test)
rf_test <- rf_test[, 2]

#classifierplots::density_plot(test$y, rf_test)
ytest <- as.numeric(ifelse(test$y == "1", 1, 0))
MLmetrics::AUC(rf_test, ytest) #.81 to beat

rftest_corrected <- corrected_probability(result = rf_test,
                                         population_fraction = 0.04,
                                         sample_fraction = 0.33)
MLmetrics::LogLoss(rftest_corrected, ytest) #0.14 to beat

#tabb <- InformationValue::confusionMatrix(ytest, rftest_corrected, threshold = 0.0238)
# randomForest::partialPlot(forest_fit2, pred.data = x_reduced, x.var = "V141")
# 
# par.Petal_W <- pdp::partial(forest_fit2, pred.var = c("V141"), train = x_reduced)
# ggplot2::autoplot(par.Petal_W, contour = TRUE)

#joint AUC plots
pred.rf <- ROCR::prediction(rftest_corrected, test$y)
perf.rf <- ROCR::performance(pred.rf, "tpr", "fpr") 
ROCR::plot(perf.rf, main = "ROC", col = 1) 

pred.earth <- ROCR::prediction(test_pred, test$y)
perf.earth <- ROCR::performance(pred.earth, "tpr", "fpr") 
ROCR::plot(perf.earth, col = 2, add = TRUE) 
legend(0.6, 0.6, c("RF", "MARS"), 1:2)

# XGBOOST -----------------------------------------------------------------

grid = expand.grid(
  nrounds = 100,
  colsample_bytree = 1,
  min_child_weight = 1,
  eta = c(0.1, 0.3, 0.5), #0.3 is default,
  gamma = c(0.25, 0.5),
  subsample = 1,
  max_depth = c(3)
)

# grid

cntrl = caret::trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "final"
)

set.seed(123)
train.xgb = caret::train(
  x = x_reduced,
  y = y,
  trControl = cntrl,
  tuneGrid = grid,
  method = "xgbTree",
  metric = "Kappa"
)

train.xgb

param <- list( objective = "binary:logistic",
               booster = "gbtree",
               eval_metric = "error",
               eta = 0.5,
               max_depth = 3,
               subsample = 1,
               colsample_bytree = 1,
               gamma = 0.25
)

train.mat <- xgboost::xgb.DMatrix(data = x_reduced, label = ynum)

set.seed(1232)
xgb.fit <- xgboost::xgb.train(params = param, data = train.mat, nrounds =
                                100)
impMatrix <- xgboost::xgb.importance(feature_names = dimnames(x_reduced)[[2]],
                                     model = xgb.fit)

xgboost::xgb.plot.importance(impMatrix, main = "Gain by Feature")

pred <- predict(xgb.fit, x_reduced) 

#classifierplots::density_plot(y, pred)

MLmetrics::AUC(pred, y) #.88
ynum <- as.numeric(ifelse(y == "1", 1, 0))
MLmetrics::LogLoss(pred, ynum) #.12

#InformationValue::optimalCutoff(ynum, pred, optimiseFor = "Both") # 0.037

test_xgb <- as.matrix(test)
test_xgb <- test_xgb[, my_forest_vars]

xgb_test_matrix <- xgboost::xgb.DMatrix(data = test_xgb, label = ytest) 
xgb_pred <- predict(xgb.fit, xgb_test_matrix)

# classifierplots::density_plot(ytest, xgb_pred)

Metrics::auc(ytest, xgb_pred) #.83

MLmetrics::LogLoss(xgb_pred, ytest) #.138

# compare
ROCR::plot(perf.rf, main = "ROC", col = "black") 

ROCR::plot(perf.earth, col = "red", add = TRUE) 

pred.xgb <- ROCR::prediction(xgb_pred, test$y)
perf.xgb <- ROCR::performance(pred.xgb, "tpr", "fpr")
ROCR::plot(perf.xgb, col = "green", add = TRUE)
legend(x = .75, y = .5, 
       legend = c("RF", "MARS", "XGB"), 
       fil = c("black", "red", "green"),
       col = c(1,2,3))

# RF feature selection ----------------------------------------------------
set.seed(1066)
index <- caret::createDataPartition(sim_df$y, p = 0.7, list = F)
train <- sim_df[index, ]
test <- sim_df[-index, ]


set.seed(5150)
rf_fs <- Boruta::Boruta(y ~ ., data = train)

rf_fs$timeTaken #22.15982

# saveRDS(rf_fs, file = 'rf_fs.RData')

table(rf_fs$finalDecision)

fnames <- Boruta::getSelectedAttributes(rf_fs) #withTentative = TRUE

fnames

boruta_train <- train[, colnames(train) %in% fnames]

boruta_train$y <- train$y

set.seed(999)
boruta_fit <- randomForest::randomForest(y ~ ., data = train)

# boruta_fit

boruta_pred <- predict(boruta_fit, type = "prob", newdata = test)
boruta_pred <- boruta_pred[, 2]

# classifierplots::density_plot(test$y, boruta_pred)
ytest <- as.numeric(ifelse(test$y == "1", 1, 0))
MLmetrics::AUC(boruta_pred, ytest) 
MLmetrics::LogLoss(boruta_pred, ytest) 

#rf_fs <- readRDS("rf_fs.RData")
