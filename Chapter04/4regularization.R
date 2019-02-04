# Regularization ----------------------------------------------------------
library(magrittr)
install.packages("glmnet")
install.packages("caret")
install.packages("classifierplots")
install.packages("DataExplorer")
install.packages("InformationValue")
install.packages("Metrics")
install.packages("ROCR")
install.packages("tidyverse")
options(scipen=999)

# data creation -----------------------------------------------------------

set.seed(999)
sim <-
  caret::twoClassSim(
    n = 10000,
    intercept = -8,
    linearVars = 6,
    noiseVars = 4
  )
table(sim$Class)

sim_df <- sim

set.seed(1)
class1 <- 10 + rnorm(7072, 1, .75)
set.seed(2)
class2 <- 9.75 + rnorm(2928, 1, .75)
classF <- ifelse(sim_df$Class == 'Class1', class1, class2)
boxplot(classF ~ sim_df$Class)
sim_df$random1 <- classF

set.seed(3)
rnd <- rnorm(10000, 10, 0.5)
sim_df$Linear6 <- sim_df$Linear5 + rnd
sim_df$Linear6 <- scale(sim_df$Linear6)

sim_df$y <- ifelse(sim_df$Class == 'Class2', 1, 0)
table(sim_df$y)
sim_df <- sim_df[, -16]
colnames(sim_df)

readr::write_csv(sim_df, "sim_df.csv")
# Data Exploration --------------------------------------------------------
sim_df <- readr::read_csv('sim_df.csv')

colnames(sim_df)

table(sim_df$y)

DataExplorer::plot_correlation(sim_df)

set.seed(1066)
index <- caret::createDataPartition(sim_df$y, p = 0.7, list = F)
train <- sim_df[index, ]
test <- sim_df[-index, ]

x <- as.matrix(train[, -17])
y <- as.factor(train$y)

set.seed(1999)
ridge <- glmnet::cv.glmnet(
  x,
  y,
  nfolds = 5,
  type.measure = "auc",
  alpha = 0,
  family = "binomial"
) 

plot(ridge)

ridge$lambda.min  
ridge$lambda.1se

coef(ridge, s = "lambda.1se")

ridge_pred <-
  data.frame(predict(ridge, newx = x, type = "response", s = "lambda.1se"))

classifierplots::density_plot(y, ridge_pred$X1)

Metrics::auc(y, ridge_pred$X1)


ridge_test <-
  data.frame(predict(ridge, newx = as.matrix(test[, -17]),
                     type = 'response'), s = "lambda.1se")
Metrics::auc(test$y, ridge_test$X1) 
Metrics::logLoss(test$y, ridge_test$X1) 
classifierplots::density_plot(test$y, ridge_test$X1)


set.seed(1876)
lasso <- glmnet::cv.glmnet(
  x,
  y,
  nfolds = 5,
  type.measure = "auc",
  alpha = 1,
  family = "binomial"
)

plot(lasso)

coef(lasso, s = "lambda.1se")

lasso_pred <-
  data.frame(predict(
    lasso,
    newx = x,
    type = "response",
    s = "lambda.1se"
  ))

Metrics::auc(y, lasso_pred$X1) 

classifierplots::density_plot(y, lasso_pred$X1)

lasso_test <-
  data.frame(predict(lasso, newx = as.matrix(test[, -17]), type = 'response'), s = "lambda.1se")

Metrics::auc(test$y, lasso_test$X1)

Metrics::logLoss(test$y, lasso_test$X1)

classifierplots::density_plot(test$y, lasso_test$X1)

grid <-
  expand.grid(.alpha = seq(0, 1, by = .2),
              .lambda = seq(0.01, 0.03,  by = 0.002))
head(grid)

control <- caret::trainControl(method = 'cv', number = 5)

set.seed(2222)
enet <- caret::train(x,
                     y,
                     method = "glmnet",
                     trControl = control,
                     tuneGrid = grid,
                     metric = "Kappa")
# enet
# ridgeTrain$results
enet$bestTune

best_enet <- glmnet::glmnet(x,
                            y,
                            alpha = 0.4,
                            lambda = 0.01,
                            family = "binomial")
# no plot
coef(best_enet)

enet_pred <- predict(enet, train, type = "prob")

Metrics::auc(y, enet_pred$`1`)

classifierplots::density_plot(y, enet_pred$`1`)

enet_test <-
  predict(enet, test, type = "prob")

Metrics::auc(test$y, enet_test$`1`)
Metrics::logLoss(test$y, enet_test$`1`)
classifierplots::density_plot(test$y, enet_test$`1`)

# ROC
pred.ridge <- ROCR::prediction(ridge_test$X1, test$y)
perf.ridge <- ROCR::performance(pred.ridge, "tpr", "fpr") 
ROCR::plot(perf.ridge, main = "ROC", col = 1) 

pred.lasso <- ROCR::prediction(lasso_test$X1, test$y)
perf.lasso <- ROCR::performance(pred.lasso, "tpr", "fpr") 
ROCR::plot(perf.lasso, col = 2, add = TRUE) 

pred.enet <- ROCR::prediction(enet_test$'1', test$y)
perf.enet <- ROCR::performance(pred.enet, "tpr", "fpr") 
ROCR::plot(perf.enet, col = 3, add = TRUE) 

legend(0.6, 0.6, c("Ridge", "LASSO", "ENET"), 1:3)
