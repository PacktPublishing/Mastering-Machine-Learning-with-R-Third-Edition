library(magrittr)
install.packages("earth")
install.packages("glmnet")
install.packages("mlr")
install.packages("randomForest")
install.packages("tidyverse")

dna <- read.csv("dna.csv")
table(dna$Class)

#3186 x 181
dupes <- duplicated(dna)
# table(dupes)
# table(dupes, dna$Class)

na_count <-
  sapply(dna, function(y)
    sum(length(which(is.na(
      y
    )))))
table(na_count)

feature_variance <- caret::nearZeroVar(dna[, -181], saveMetrics = TRUE)
table(feature_variance$zeroVar)

high_corr <- caret::findCorrelation(dna[, -181], cutoff = 0.9)
length(high_corr)

# dna_nocorr <- dna[, -high_corr]

set.seed(555)
index <- caret::createDataPartition(y = dna$Class, p = 0.8, list = FALSE)
train <- dna[index, ]
test <- dna[-index, ]

dna_task <- mlr::makeClassifTask(data = train, target = "Class")

# str(mlr::getTaskData(dna_task))

rdesc <- mlr::makeResampleDesc("Subsample", iters = 5)
param <-
  ParamHelpers::makeParamSet(ParamHelpers::makeDiscreteParam("ntree", values = c(50, 75, 100, 150, 175, 200)))

ctrl <- mlr::makeTuneControlGrid() 

library(mlr)
tuning <-
  mlr::tuneParams(
    "classif.randomForest",
    task = dna_task,
    resampling = rdesc,
    par.set = param,
    control = ctrl)

tuning$x
tuning$y

rf <-
  mlr::setHyperPars(mlr::makeLearner("classif.randomForest", predict.type = "prob"),
               par.vals = tuning$x)

fit_rf <- mlr::train(rf, dna_task) 
fit_rf$learner.model 

pred <- predict(fit_rf, newdata = test)
mlr::calculateConfusionMatrix(pred) 
mlr::performance(pred, measures = list(acc, logloss))


# mlr ensemble ------------------------------------------------------------

base <- c("classif.randomForest", "classif.earth")

learns <- lapply(base, makeLearner)

learns <- lapply(learns, setPredictType, "prob")

sl <-
  mlr::makeStackedLearner(
    base.learners = learns,
    super.learner = "classif.glmnet",
    predict.type = "prob",
    method = "stack.cv"
  ) 

stacked_fit <- mlr::train(sl, dna_task)
pred_stacked <- predict(stacked_fit, newdata = test)
mlr::calculateConfusionMatrix(pred_stacked)
mlr::performance(pred_stacked, measures = list(acc, logloss))

