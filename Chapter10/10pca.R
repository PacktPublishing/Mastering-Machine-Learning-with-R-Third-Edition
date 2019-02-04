library(magrittr)
# install.packages("caret")
# install.packages("earth")
# install.packages("psych")
# install.packages("tidyverse")

options(scipen = 999)

army_ansur <- readRDS("army_ansur.RData")

colnames(army_ansur)
table(army_ansur$Component)
table(army_ansur$Gender)

sapply(army_ansur, function(x) sum(is.na(x)))

army_ansur$subjectid <- seq(1:6068)
# janitor::get_dupes(army)

sjmisc::descr(army_ansur$Weightlbs)

ggplot2::ggplot(army_ansur, ggplot2::aes(x = Weightlbs)) + 
  ggplot2::geom_density() +
  ggthemes::theme_wsj()

dplyr::select(army_ansur, Weightlbs) %>%
  dplyr::arrange(Weightlbs)

armyClean <- dplyr::filter(army_ansur, Weightlbs > 0)

army_subset <- armyClean[, c(1:91, 93, 94, 106, 107)]

# train and test
set.seed(1812)
army_subset %>%
  dplyr::sample_frac(.8) -> train

army_subset %>%
  dplyr::anti_join(train, by = "subjectid") -> test

# index <- train$subjectid 

DataExplorer::plot_correlation(train[, 2:6])

trainY <- train$Weightlbs

train_scale <- data.frame(scale(train[, c(-1, -95)]))

pca <- psych::principal(train_scale, rotate = "none") 

plot(pca$values[1:10], type = "b", ylab = "Eigenvalues", xlab = "Component")
head(pca$values, 10)
# let's go with 5

sum(pca$values)
sum(pca$values[1:5])

pca_5 <- psych::principal(train_scale, nfactors = 5, rotate = "none")
pca_5

pca_rotate <- psych::principal(train_scale, nfactors = 5, rotate = "varimax")

# pca_rotate
pca_loadings <- unclass(pca_rotate$loadings)
pca_loadings <- data.frame(pca_loadings)
pca_loadings$features <- row.names(pca_loadings)
readr::write_csv(pca_loadings, "pca_loadings.csv")

pca_scores <- data.frame(round(pca_5$scores, digits = 2))
head(pca_scores)
pca_scores$weight <- trainY

DataExplorer::plot_correlation(pca_scores)

# pca_lm <- lm(weight ~ ., data = pca_scores)
# 
# broom::tidy(pca_lm)
# 
# summary(pca_lm)
# 
# pca_scores$predicted <- round(pca_lm$fitted.values, digits = 3)
# 
# ggplot2::ggplot(pca_scores, ggplot2::aes(x = predicted, y = weight)) +
#   ggplot2::geom_point() +
#   ggplot2::stat_smooth(method = "lm", se = FALSE)  +
#   ggthemes::theme_pander()
# 
# pca_scores$residuals <- round(pca_lm$residuals, digits = 2)
# 
# ggplot2::ggplot(pca_scores, ggplot2::aes(x = predicted, y = residuals)) +
#   ggplot2::geom_point() +
#   ggplot2::stat_smooth(method = "loess", se = FALSE)  +
#   ggthemes::theme_few()
# 
# caret::postResample(pred = pca_scores$predicted, 
#                     obs = pca_scores$weight)
# 
# lm_test_pred <- predict(pca_lm, test)
# 
# caret::postResample(pred = pca_scores$predicted, 
#                     obs = pca_scores$weight)

# colnames(pca_scores)

set.seed(1492)
earth_fit <-
  earth::earth(
    x = pca_scores[, 1:5],
    y = pca_scores[, 6],
    pmethod = 'cv',
    nfold = 10,
    degree = 1,
    minspan = -1
  )

summary(earth_fit) 

plotmo::plotmo(earth_fit)

pca_scores$earthpred <- predict(earth_fit)

ggplot2::ggplot(pca_scores, ggplot2::aes(x = earthpred, y = weight)) +
  ggplot2::geom_point() +
  ggplot2::stat_smooth(method = "lm", se = FALSE)  +
  ggthemes::theme_pander()

pca_scores$earthresid <- earth_fit$residuals

ggplot2::ggplot(pca_scores, ggplot2::aes(x = earthpred, y = earthresid)) +
  ggplot2::geom_point() +
  ggplot2::stat_smooth(method = "loess", se = FALSE)  +
  ggthemes::theme_few()

caret::postResample(pred = pca_scores$earthpred, 
                    obs = pca_scores$weight)
# ggplot2::ggplot(pca_scores, ggplot2::aes(x = earthresid)) + 
#   ggplot2::geom_density() +
#   ggthemes::theme_tufte()
# test set predict

test_reduced <- as.matrix(test[, c(-1, -95)])
test_scores <- data.frame(predict(pca_5, test_reduced, old.data = train[, c(-1, -95)]))

test_scores$testpred <- predict(earth_fit, test_scores)

test_scores$weight <- test$Weightlbs

caret::postResample(pred = test_scores$testpred, 
                    obs = test_scores$weight)

ggplot2::ggplot(test_scores, ggplot2::aes(x = testpred, y = weight)) +
  ggplot2::geom_point() +
  ggplot2::stat_smooth(method = "lm", se = FALSE)  +
  ggthemes::theme_excel_new()

test_scores$testresid <- test_scores$weight - test_scores$testpred

ggplot2::ggplot(test_scores, ggplot2::aes(x = testpred, y = testresid)) +
  ggplot2::geom_point() +
  ggplot2::stat_smooth(method = "loess", se = FALSE)  +
  ggthemes::theme_few()
