

data(anscombe)

attach(anscombe)
anscombe

cor(x1, y1) #correlation of x1 and y1

cor(x2, y1) #correlation of x2 and y2

par(mfrow = c(2,2)) #create a 2x2 grid for plotting

plot(x1, y1, main = "Plot 1")

plot(x2, y2, main = "Plot 2")

plot(x3, y3, main = "Plot 3")

plot(x4, y4, main = "Plot 4")

install.packages("alr3")

library(alr3)

data(snake)

dim(snake)

head(snake)

colnames(snake) <- c("content", "yield")

attach(snake) # attach data with new names

head(snake)


plot(content,
     yield,
     main = "Scatterplot of Snow vs. Yield",
     xlab = "water content of snow",
     ylab = "water yield")

yield_fit <- lm(yield ~ content)

summary(yield_fit)

plot(content, log(yield))

abline(yield_fit, lwd = 3, col = "red")

par(mfrow = c(2,2))
plot(yield_fit)

# multivariate
library(magrittr)
options(scipen = 999)
options(digits = 4)
# install.packages("caret")
# install.packages("ggthemes")
# install.packages("janitor")
# install.packages("leaps")
# install.packages("MASS")
# install.packages("plm")
# install.packages("readr")
# install.packages("sjmisc")
# install.packages("tidyverse")
# install.packages("vtreat")

ames <- readr::read_csv("C:/Users/cory/Desktop/data/ames.csv")

dim(ames)

dupes <- duplicated(ames)
table(dupes)

ames %>%
  sjmisc::descr() -> ames_descr

range(ames$Id)
#Id
ames <- ames[, -1]
#overall quality and condition
# sales price
# age
ames %>%
  dplyr::mutate(yearsOld = YrSold - YearBuilt) -> ames
# year remod
ames %>%
  dplyr::mutate(yearsRemodel = YrSold - YearRemodAdd) -> ames

# garage year built
ames %>%
  dplyr::mutate(yearsGarage = YrSold - GarageYrBlt) -> ames
# missing
ames$yearsGarage_isNA <- 
  ifelse(is.na(ames$yearsGarage), 1, 0)

ames$yearsGarage[is.na(ames$yearsGarage)] <- 0
# MoSold character

# remove
ames <- ames[, c(-19, -20, -59)]

ames$MoSold <- as.character(ames$MoSold)

ggplot2::ggplot(ames, ggplot2::aes(x = SalePrice)) + 
  ggplot2::geom_histogram() +
  ggthemes::theme_few()

ames %>%
  dplyr::mutate(logSales = log(SalePrice)) -> ames

ggplot2::ggplot(ames, ggplot2::aes(x = logSales)) + 
  ggplot2::geom_histogram() +
  ggthemes::theme_economist_white()

ames$LotFrontage_isNA <- 
  ifelse(is.na(ames$LotFrontage), 1, 0)

ames$LotFrontage[is.na(ames$LotFrontage)] <- 0

ames$MasVnrArea_isNA <- 
  ifelse(is.na(ames$MasVnrArea), 1, 0)

ames$MasVnrArea[is.na(ames$MasVnrArea)] <- 0

# low or no variance ------------------------------------------------------

feature_variance <- caret::nearZeroVar(ames, saveMetrics = TRUE)

table(feature_variance$zeroVar)
#Street

# train and test
set.seed(1944)

ames %>%
  dplyr::sample_frac(.8) -> train

ames %>%
  dplyr::anti_join(train) -> test

varlist = colnames(ames[, !colnames(ames) %in% c('SalePrice', 'logSales')])

train_y <- train$SalePrice
train_logy <- train$logSales
test_y <- test$SalePrice
test_logy <- test$logSales

# design treatments
df_treatment <- vtreat::designTreatmentsZ(
  dframe = train,
  varlist = varlist,
  minFraction = 0.10
)

trained <- vtreat::prepare(df_treatment, train)
tested <- vtreat::prepare(df_treatment, test)

trained <- 
  trained %>%
  dplyr::select(-dplyr::contains('_catP'))

tested <- 
  tested %>%
  dplyr::select(-dplyr::contains('_catP'))

colnames(trained) <-
  sub('_clean', "", colnames(trained))

colnames(tested) <-
  sub('_clean', "", colnames(tested))

colnames(trained) <-
  sub('_isBAD', "_isNA", colnames(trained))

colnames(tested) <-
  sub('_isBAD', "_isNA", colnames(tested))

# correlation
df_corr <- cor(trained)

high_corr <- caret::findCorrelation(df_corr, cutoff = 0.79)

length(high_corr)

trained <- trained[, -high_corr]

caret::findLinearCombos(trained)

# linear dependency
plm::detect_lin_dep(trained)

# fit stepwise
step_control <-
  caret::trainControl(method = "cv",
                      number = 3,
                      returnResamp = "final")

set.seed(1984)
step_fit <-
  caret::train(trained, train_logy, method =  "leapForward",  
               tuneGrid = data.frame(nvmax = 10:25), 
               trControl = step_control,
               trace = FALSE)

# step_fit$results
step_fit$bestTune

#coef(step_fit$finalModel)
broom::tidy(coef(step_fit$finalModel, 20)) -> step_coef

View(step_coef)

step_coef <- step_coef[-1, ]
lm_formula <- as.formula(paste("y ~ ",paste(step_coef$names, collapse="+"), sep = ""))

trained$y <- train_logy
step_lm <- lm(lm_formula, data = trained)

# summary(step_lm)
# broom::tidy(step_lm)
broom::glance(step_lm)
par(mfrow = c(2,2))
plot(step_lm) 

train_reduced <- trained[c(-87, -248, -918), ] 

step_lm2 <- lm(lm_formula, data = train_reduced)
# plot(step_lm2)
car::qqPlot(step_lm2$residuals)

step_vif <- broom::tidy(car::vif(step_lm2))

View(step_vif)

step_pred <- predict(step_lm2, tested)
caret::postResample(pred = step_pred, obs = test_logy)
caret::postResample(step_lm2$fitted.values, train_reduced$y)
# 
# step_resid <- (test_logy - step_pred)
# car::qqPlot(step_resid)

# Earth -------------------------------------------------------------------
# colnames(train_reduced)
set.seed(1988)
earth_fit <-
  earth::earth(
    x = train_reduced[, -96],
    y = train_reduced[, 96],
    pmethod = 'cv',
    nfold = 5,
    degree = 1,
    minspan = -1,
    nprune = 25
  )

summary(earth_fit) 
# plot(earth_fit)
earth::evimp(earth_fit)
plotmo::plotmo(earth_fit, nrug = TRUE, rug.col = "red")

# earth_resid <- earth_fit$residuals
# hist(earth_resid)
# qqnorm(earth_resid); qqline(earth_resid, col = "red")

earth_pred <- predict(earth_fit, tested)
caret::postResample(earth_pred, test_logy)

earth_residTest <- test_logy - earth_pred

car::qqPlot(earth_residTest)
 
earth_predExp <- exp(earth_pred)
caret::postResample(earth_predExp, test_y) 

# Duan's smearing function
# expo_earth_resid <- exp(earth_resid)
# expo_earth_pred <- exp(earth_pred)
# hist(expo_earth_resid)
# avg_expo_resid <- mean(expo_earth_resid)


duan_smear <- function(pred, resid){
  expo_resid <- exp(resid)
  expo_pred <- exp(pred)
  avg_expo_resid <- mean(expo_resid)
  smear_predictions <- avg_expo_resid * expo_pred
  return(smear_predictions)
}

duan_pred <- duan_smear(pred = earth_pred, resid = earth_residTest)

caret::postResample(duan_pred, test_y)

exp_pred <- exp(earth_pred)
caret::postResample(exp_pred, test_y)

results <- data.frame(exp_pred, test_y)
colnames(results) <- c('predicted', 'actual')

ggplot2::ggplot(results, ggplot2::aes(predicted, actual)) +
  ggplot2::geom_point(size=1) +
  ggplot2::geom_smooth() +
  ggthemes::theme_fivethirtyeight()
