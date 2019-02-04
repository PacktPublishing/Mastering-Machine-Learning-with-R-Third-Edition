library(magrittr)

army <- readr::read_csv("C:/Users/ps324/OneDrive - Cummins/documents/Explore/army.csv")

saveRDS(army, "army1.RData")

army_f <- readr::read_csv("C:/Users/ps324/OneDrive - Cummins/documents/Explore/army_f.csv")

saveRDS(army_f, "army2.RData")

colnames(army)
table(army$Component)

sapply(army, function(x) sum(is.na(x)))

janitor::get_dupes(army)

sjmisc::descr(army$Weightlbs)

ggplot2::ggplot(army, ggplot2::aes(x = Weightlbs)) + 
  ggplot2::geom_density() +
  ggthemes::theme_gdocs()

dplyr::select(army, Weightlbs) %>%
  dplyr::arrange(Weightlbs)

armyClean <- dplyr::filter(army, Weightlbs > 0)

#janitor::clean_names(army, case = "")

army_subset <- armyClean[, c(2:94,106)]

# train and test

DataExplorer::plot_correlation(army_subset[, 1:10])

army_scale <- data.frame(scale(army_subset))

pca <- psych::principal(army_scale, rotate = "none") 

plot(pca$values, type = "b", ylab = "Eigenvalues", xlab = "Component")
head(pca$values, 10)
# let's go with 5

sum(pca$values)
sum(pca$values[1:6])

pca_6 <- psych::principal(army_scale, nfactors = 6, rotate = "none")
pca_6

pca_scores <- data.frame(round(pca_6$scores, digits = 2))
head(pca_scores)

pca_scores$weight <- armyClean$Weightlbs

# pca_scores$regulararmy <- ifelse(armyClean$Component == "Regular Army", 1, 0)

DataExplorer::plot_correlation(pca_scores)

pca_lm <- lm(weight ~ ., data = pca_scores)

broom::tidy(pca_lm)

summary(pca_lm)

pca_scores$predicted <- round(pca_lm$fitted.values, digits = 2)

ggplot2::ggplot(pca_scores, ggplot2::aes(x = predicted, y = weight)) +
  ggplot2::geom_point() +
  ggplot2::stat_smooth(method = "lm", se = FALSE)  +
  ggthemes::theme_pander()

pca_scores$residuals <- round(pca_lm$residuals, digits = 2)

ggplot2::ggplot(pca_scores, ggplot2::aes(x = predicted, y = residuals)) +
  ggplot2::geom_point() +
  ggplot2::stat_smooth(method = "lm", se = FALSE)  +
  ggthemes::theme_few()

plot(pca_lm)

pca_trimmed <- pca_scores[c(-2388, -3668, -3918), c(-8, -9)]

pca_lm2 <- lm(weight ~ ., data = pca_trimmed)

summary(pca_lm2)

plot(pca_lm2)
