library(magrittr)
install.packages("caret")
install.packages("classifierplots")
install.packages("corrplot")
install.packages("earth")
install.packages("Information")
install.packages("InformationValue")
install.packages("Metrics")
install.packages("ROCR")
install.packages("tidyverse")

santander <- read.csv("santander_prepd.csv")
dim(santander)
table(santander$y)

set.seed(1966)

trainIndex <- caret::createDataPartition(santander$y, p = 0.8, list = FALSE)
train <- santander[trainIndex, ]
test <- santander[-trainIndex, ]

table(train$y)
table(test$y)

train_zero <- caret::nearZeroVar(train, saveMetrics = TRUE)
table(train_zero$zeroVar)
train <- train[, train_zero$zeroVar == 'FALSE']


# WOE and IV calculation example ------------------------------------------
bin1events <- 4
bin1nonEvents <- 96
bin2events <- 12
bin2nonEvents <- 88
totalEvents <- bin1events + bin2events
totalNonEvents <- bin1nonEvents + bin2nonEvents

bin1percentE <- bin1events / totalEvents
bin1percentNE <- bin1nonEvents / totalNonEvents

bin2percentE <- bin2events / totalEvents
bin2percentNE <- bin2nonEvents / totalNonEvents

bin1WOE <- log(bin1percentE / bin1percentNE)
bin2WOE <- log(bin2percentE / bin2percentNE)

bin1IV <- (bin1percentE - bin1percentNE) * bin1WOE
bin2IV <- (bin2percentE - bin2percentNE) * bin2WOE

bin1IV + bin2IV

options(scipen = 999)
options(digits = 4)

IV <- Information::create_infotables(data = train,
                                     y = "y",
                                     parallel = FALSE)

knitr::kable(head(IV$Summary, 25)) 
knitr::kable(IV$Tables$V2)
Information::plot_infotables(IV, "V2", show_values = TRUE)
Information::plot_infotables(IV, IV$Summary$Variable[1:4], same_scales=TRUE)

features <- IV$Summary$Variable[1:21]

train_reduced <- train[, colnames(train) %in% features]
train_reduced$y <- train$y

glm_control <-
  caret::trainControl(method = "cv",
                      number = 5,
                      returnResamp = "final"
                      
                      )

x <- train_reduced[, -22]
y <- as.factor(train_reduced$y)
set.seed(1988)
glm_fit <-
  caret::train(x, y, method =  "glm",    
               trControl = glm_control,
               trace = FALSE)

glm_fit$results

glm_train_pred <- predict(glm_fit, train, type = "prob")
colnames(glm_train_pred) <- c("zero", "one")
classifierplots::density_plot(train_reduced$y, glm_train_pred$one)

glm_cutoff <-
  InformationValue::optimalCutoff(
    train_reduced$y,
    glm_train_pred$one,
    optimiseFor = 'Both',
    returnDiagnostics = TRUE
  )
InformationValue::confusionMatrix(train_reduced$y, glm_train_pred$one, threshold = 0.0607)

caret::varImp(glm_fit)

# glm_fit$finalModel

glm_test_pred <- predict(glm_fit, test, type = "prob")
colnames(glm_test_pred) <- c("zero", "one")
classifierplots::density_plot(test$y, glm_test_pred$one)

InformationValue::confusionMatrix(test$y, glm_test_pred$one, threshold = 0.0607)
Metrics::auc(test$y, glm_test_pred$one)
Metrics::logLoss(test$y, glm_test_pred$one)

# MARS
colnames(train)
set.seed(1972)
earth_fit <-
  earth::earth(
    x = train[, -142],
    y = train[, 142],
    pmethod = 'cv',
    #'cv'
    nfold = 5,
    # ncross = 1,
    degree = 1,
    minspan = -1,
    nprune = 15,
    glm = list(family = binomial)
  )

summary(earth_fit) 

plotmo::plotmo(earth_fit, ylim = NA)

earth::evimp(earth_fit)

earth::plotd(earth_fit)

pred <- predict(earth_fit, train, type = 'response')

mars_cutoff <-
  InformationValue::optimalCutoff(
    train$y,
    pred,
    optimiseFor = 'Both',
    returnDiagnostics = TRUE
  )
InformationValue::confusionMatrix(train$y, pred, threshold = 0.05)

test_pred <- predict(earth_fit, test, type = 'response')
Metrics::auc(test$y, test_pred)
Metrics::logLoss(test$y, test_pred)

# joint AUC plots
pred.glm <- ROCR::prediction(glm_test_pred$one, test$y)
perf.glm <- ROCR::performance(pred.glm, "tpr", "fpr") 
ROCR::plot(perf.glm, main = "ROC", col = 1) 

pred.earth <- ROCR::prediction(test_pred, test$y)
perf.earth <- ROCR::performance(pred.earth, "tpr", "fpr") 
ROCR::plot(perf.earth, col = 2, add = TRUE) 
legend(0.6, 0.6, c("GLM", "MARS"), 1:2)

# gain chart
# perf.gain.glm <- ROCR::performance(pred.glm, "tpr", "rpp")
# ROCR::plot(perf.gain.glm, main = "Gain Chart")
# perf.gain.earth <- ROCR::performance(pred.earth, "tpr", "rpp")
# ROCR::plot(perf.gain.earth, col = 2, add = TRUE)
# legend(0.6, 0.6, c("GLM", "MARS"), 1:2)
