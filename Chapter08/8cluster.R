cc_cluster <- readr::read_csv("C:/Users/ps324/OneDrive - Cummins/documents/Explore/cc_cluster.csv")

library(magrittr)
install.packages("flexclust")


colnames(cc_cluster)

sjmisc::count_na(cc_cluster)

sapply(cc_cluster, function(x) sum(is.na(x)))

cc_cluster[is.na(cc_cluster)] <- 0

# run a density plot of a feature
##


table(cc_cluster$TENURE)

cc_scale <- data.frame(scale(cc_cluster[, -1]))

cc_dist <- dist(cc_scale, method = "euclidean") 

set.seed(123)
# Compute and plot wss for k = 2 to k = 6
k.max <- 12 # Maximal number of clusters
data <- cc_scale
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=10 )$tot.withinss})

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#abline(v = 3, lty =2)

hc <- hclust(cc_dist, method = "ward.D2") 

plot(hc, hang = -1, labels = FALSE, main = "Ward")

ward_7 <- cutree(hc, 7) 
#ColorDendrogram(hc, y = ward5, main = "Five Segments", branchlength = 50)

cc_scale$hc7 <- ward_7
table(cc_scale$hc7)

cc_scale %>% 
  dplyr::group_by(hc7) %>%
  dplyr::summarise_all(.funs = mean) -> hc_clusters

# pareto chart?

set.seed(1234)
km <- kmeans(cc_scale[, c(-18, -19)], 6, nstart = 25) 

table(km$cluster)
km_clusters <- data.frame(km$centers)

set.seed(4321)

# DBScan ------------------------------------------------------------------


                       