library(magrittr)
install.packages("cluster") 
install.packages("dendextend")
install.packages("ggthemes")
install.packages("HDclassif") 
install.packages("NbClust") 
install.packages("tidyverse")
options(scipen=999)

library(HDclassif)
data(wine)
str(wine)
colnames(wine) <- c(
  "Class",
  "Alcohol",
  "MalicAcid",
  "Ash",
  "Alk_ash",
  "magnesium",
  "T_phenols",
  "Flavanoids",
  "Non_flav",
  "Proantho",
  "C_Intensity",
  "Hue",
  "OD280_315",
  "Proline"
)

wine_df <- as.data.frame(scale(wine[, -1]))

table(wine$Class)

numComplete <- NbClust::NbClust(
  wine_df,
  distance = "euclidean",
  min.nc = 2,
  max.nc = 6,
  method = "complete",
  index = "all"
)

numComplete$Best.nc

euc_dist <- dist(wine_df, method = "euclidean")

hc_complete <- hclust(euc_dist, method = "complete")

dend_complete <- as.dendrogram(hc_complete)
dend1 <- dendextend::color_branches(dend_complete, k = 3)
plot(dend1, main = "Complete-Linkage")

complete_clusters <- cutree(hc_complete, 3)
table(complete_clusters)

table(complete_clusters, wine$Class)

numWard <- NbClust::NbClust(
  wine_df,
  distance = "euclidean",
  min.nc = 2,
  max.nc = 6,
  method = "ward.D2",
  index = "all"
)

hc_ward <- hclust(euc_dist, method = "ward.D2")

dend_ward <- as.dendrogram(hc_ward)
dend2 <- dendextend::color_branches(dend_ward, k = 3)
plot(dend2, main = "Ward Method")

ward_clusters <- cutree(hc_ward, 3)

table(ward_clusters, wine$Class)

table(complete_clusters, ward_clusters)

ward_df <- wine_df %>%
  dplyr::mutate(cluster = ward_clusters)

ward_df %>%
  dplyr::group_by(cluster) %>%
  dplyr::summarise_all(dplyr::funs(mean)) -> ward_results

ggplot2::ggplot(ward_results, ggplot2::aes(cluster, Alcohol)) +
  ggplot2::geom_bar(stat = "identity") +
  ggthemes::theme_stata()


numKMeans <- NbClust::NbClust(wine_df,
                     min.nc = 2,
                     max.nc = 15,
                     method = "kmeans")

set.seed(1234)

km <- kmeans(wine_df, 3, nstart = 25)

table(km$cluster)

par(mfrow = c(1, 2))

boxplot(wine$Alcohol ~ km$cluster, data = wine, 
          main = "Alcohol Content, K-Means")

boxplot(wine$Alcohol ~ ward_clusters, data = wine, 
          main = "Alcohol Content, Ward's")

table(km$cluster, wine$Class)

wine$Alcohol <- as.factor(ifelse(wine_df$Alcohol > 0, "High", "Low"))

gower_dist <- cluster::daisy(wine[, -1], metric = "gower")

set.seed(123)

pam_cluster <- cluster::pam(gower_dist, k = 3)

table(pam_cluster$clustering)

table(pam_cluster$clustering, wine$Class)

table(pam_cluster$clustering, wine$Alcohol)

set.seed(1918)

rf <- randomForest::randomForest(x = wine[, -1], ntree = 2000, proximity = T)

rf

dim(rf$proximity)

rf$proximity[1:5, 1:5]

randomForest::importance(rf)

rf_dist <- sqrt(1 - rf$proximity)

rf_dist[1:2, 1:2]

set.seed(1776)

pam_rf <- cluster::pam(rf_dist, k = 3)

table(pam_rf$clustering)

table(pam_rf$clustering, wine$Class)
