library(magrittr)
install.packages("ggthemes")
install.packages("caret")
install.packages("classifierplots")
install.packages("DataExplorer")
install.packages("e1071")
install.packages("InformationValue")
install.packages("kknn")
install.packages("Matrix")
install.packages("Metrics")
install.packages("plm")
install.packages("ROCR")
install.packages("tidyverse")
options(scipen=999)


download.file('https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/a5a',
              'chap5')

df <- e1071::read.matrix.csr("chap5")

saveRDS(df, file = "chapter5")

# 
df <- readRDS("chapter5")

y <- df$y

y <- ifelse(y == "+1", 1, 0)

table(y)

x <- Matrix::as.matrix(df$x)
# x <- as(mydf$x, "Matrix")

x <- as.data.frame(x)
dim(x)

# train/test split
y_factor <- as.factor(y)

set.seed(1492)
index <- caret::createDataPartition(y_factor, p = 0.7, list = F)

train <- x[index, ]
train_y <- y_factor[index]
test <- x[-index, ]
test_y <- y_factor[-index]

train_NZV <- caret::nearZeroVar(train, saveMetrics = TRUE)

table(train_NZV$nzv)

table(train_NZV$zeroVar)

# eliminate the NZV, explore the number of unique

train_r <- train[train_NZV$nzv == FALSE]

linear_combos <- caret::findLinearCombos(x = train_r)

linear_combos

train_r <- train_r[, -linear_combos$remove]

train_r <- train_r[, -5]

plm::detect_lin_dep(train_r)

my_data_cor <- cor(train_r)

high_corr <- caret::findCorrelation(my_data_cor, cutoff = 0.9)

high_corr

train_df <- train_r[, -high_corr]

DataExplorer::plot_correlation(train_df)


# KNN ---------------------------------------------------------------------

ctrl <- caret::rfeControl(
  functions = caret::lrFuncs,
  method = "cv",
  number = 10,
  verbose = TRUE
)

subsets <- c(25:35)

set.seed(1863)

knnProfile <- caret::rfe(
  train_df,
  train_y,
  sizes = subsets,
  rfeControl = ctrl,
  method = "knn",
  metric = "Kappa"
)

knnProfile #26

knn_results <- knnProfile$results

ggplot2::ggplot(knn_results, aes(Variables, Kappa)) +
  ggplot2::geom_line(color = 'darkred', size = 2) +
  ggthemes::theme_economist()

# create df
vars <- knnProfile$optVariables

x_selected <-
  train_df[, (colnames(train_df) %in% vars)]

knn_df <- cbind(x_selected, train_y)

set.seed(9954)
knn_fit <-
  kknn::train.kknn(
    train_y ~ .,
    data = knn_df,
    distance = 1,
    kmax = 25,
    kcv = 10,
    kernel = c("rectangular", "triangular", "epanechnikov")
  )

plot(knn_fit)

knn_fit

knn_pred_train <-
  data.frame(predict(knn_fit, newdata = knn_df, type = "prob"))

classifierplots::density_plot(train_y, knn_pred_train$X1)
Metrics::auc(train_y, knn_pred_train$X1)

InformationValue::optimalCutoff(train_y, knn_pred_train$X1)
# 0.48

knn_pred_test <-
  data.frame(predict(knn_fit, newdata = test, type = "prob"))

classifierplots::density_plot(test_y, knn_pred_test$X1)
Metrics::auc(test_y, knn_pred_test$X1)

pred_class <- as.factor(ifelse(knn_pred_test$X1 >= 0.48, "1", "0"))

caret::confusionMatrix(data = pred_class, reference = test_y, positive = "1")

# SVM ---------------------------------------------------------------------

ctrl <- caret::rfeControl(
  functions = caret::lrFuncs,
  method = "cv",
  number = 10,
  verbose = TRUE
)

subsets <- c(20:30)

set.seed(54321)

svmProfile <- caret::rfe(
  train_df,
  train_y,
  sizes = subsets,
  rfeControl = ctrl,
  method = "svmLinear",
  metric = "Kappa"
)

svmProfile 

# svmProfile$optVariables

svm_results <- svmProfile$results

ggplot2::ggplot(svm_results, aes(Variables, Kappa)) +
  ggplot2::geom_line(color = 'steelblue', size = 2) +
  ggthemes::theme_fivethirtyeight()

# svm_formula <- paste("y ~ ",paste(lmProfile$optVariables, collapse="+"),sep = "")

svm_vars <- svmProfile$optVariables

x_selected <-
  train_df[, (colnames(train_df) %in% svm_vars)]

grid <-
  expand.grid(.C = c(1, 2, 3))

svm_control <- caret::trainControl(method = 'cv', number = 10)

set.seed(1918)
svm <- caret::train(x_selected,
                     train_y,
                     method = "svmLinear",
                     trControl = svm_control,
                     tuneGrid = grid,
                     metric = "Kappa")

svm

svm_fit <-
  kernlab::ksvm(
    as.matrix(x_selected),
    train_y,
    kernel = "vanilladot",
    prob.model = TRUE,
    kpar = "automatic",
    C = 3
  )

svm_pred_train <-
  kernlab::predict(svm_fit, x_selected, type = "probabilities")

svm_pred_train <- data.frame(svm_pred_train)

classifierplots::density_plot(train_y, svm_pred_train$X1)

Metrics::auc(train_y, svm_pred_train$X1)

InformationValue::optimalCutoff(train_y, svm_pred_train$X1)
#0.388

# test data

test_svm <- test[, (colnames(test) %in% svm_vars)]

svm_pred_test <-
  kernlab::predict(svm_fit, test_svm, type = "probabilities")

svm_pred_test <- as.data.frame(svm_pred_test)

classifierplots::density_plot(test_y, svm_pred_test$`1`)

Metrics::auc(test_y, svm_pred_test$`1`)

svm_pred_class <- as.factor(ifelse(svm_pred_test$`1` >= 0.275, "1", "0"))

caret::confusionMatrix(data = svm_pred_class, reference = test_y, positive = "1")

# ROC
pred.knn <- ROCR::prediction(knn_pred_test$X1, test_y)
perf.knn <- ROCR::performance(pred.knn, "tpr", "fpr") 
ROCR::plot(perf.knn, main = "ROC", col = 1) 

pred.svm <- ROCR::prediction(svm_pred_test$`1`, test_y)
perf.svm <- ROCR::performance(pred.svm, "tpr", "fpr") 
ROCR::plot(perf.svm, col = 2, add = TRUE) 
legend(0.6, 0.6, c("KNN", "SVM"), 1:2)

