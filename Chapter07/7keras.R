
library(magrittr)
install.packages(caret)
install.packages(MASS)
library(MASS)
install.packages("classifierplots")
install.packages("neuralnet")
install.packages("vtreat")

#setwd("C:/Users/ps324/Desktop")

# Neural Net --------------------------------------------------------------

data("shuttle")
head(shuttle)
table(shuttle$use)


set.seed(1942)
trainIndex <-
  caret::createDataPartition(shuttle$use, p = .6, list = FALSE)
shuttleTrain <- shuttle[trainIndex, -7]
shuttleTest  <- shuttle[-trainIndex, -7] 

treatShuttle <- vtreat::designTreatmentsZ(shuttleTrain, colnames(shuttleTrain))
train_treated <- vtreat::prepare(treatShuttle, shuttleTrain)
train_treated <- train_treated[, c(-1,-2)]
test_treated <- vtreat::prepare(treatShuttle, shuttleTest)
test_treated <- test_treated[, c(-1, -2)]
shuttle_trainY <- shuttle[trainIndex, 7]
train_treated$y <- ifelse(shuttle_trainY == "auto", 1, 0)
shuttle_testY <- shuttle[-trainIndex, 7]
test_treated$y <- ifelse(shuttle_testY == "auto", 1, 0)

n <- names(train_treated)

form <- as.formula(paste("y ~", paste(n[!n %in% "y"], collapse = " + "))) 
# form
nnfit <- neuralnet::neuralnet(form, data = train_treated, err.fct = "ce", linear.output = FALSE) 

nnfit$result.matrix

# head(nnfit$generalized.weights[[1]])

plot(nnfit)
test_pred <- neuralnet::compute(nnfit, test_treated[, 1:16]) 
test_prob <- test_pred$net.result
pred <- ifelse(test_prob >= 0.5, 1, 0) 
table(pred, test_treated$y)
MLmetrics::LogLoss(test_prob, test_treated$y)

# Deep Learning -----------------------------------------------------------
install.packages("reticulate")
keras::install_keras()
library(keras)
library(reticulate)
#library(tensorflow)

#save(trained, train_logy, tested, test_logy, file = "amesDL.RData")
load("amesDL.RData")

prep <- caret::preProcess(trained, method = c("center", "scale"))
trainT <- predict(prep, trained)
train_logy <- data.matrix(train_logy)
trainT <- data.matrix(trainT)
testT <- predict(prep, tested)
testT <- data.matrix(testT)

build_model <- function() {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = dim(trainT)[2]
                ) %>%
    layer_dropout(0.3) %>%
    layer_dense(units = 64, activation = "relu",
                kernel_regularizer = regularizer_l1(l = 0.001)) %>%
    #layer_dropout(0.5) %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
  )
  
  model
}

model <- build_model()
model %>% summary()

epochs <- 100
# Fit the model and store training stats
history <- model %>% fit(
  trainT,
  train_logy,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0
)

plot(history, metrics = "mean_absolute_error", smooth = FALSE) 
min(history$metrics$mean_absolute_error)

test_predictions <- model %>% predict(testT)

MLmetrics::MAE(test_predictions, test_logy) #.24
MLmetrics::MAPE(test_predictions, test_logy)
MLmetrics::R2_Score(test_predictions, test_logy)
plot(test_predictions, test_logy)

