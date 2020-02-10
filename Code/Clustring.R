library(ISLR)
nci_labs <- NCI60$labs
nci_data <- NCI60$data
scaled_nci_data = scale(nci_data)
#1
nci_hc_complete = hclust(dist(scaled_nci_data), method = "complete")
nci_hc_average = hclust(dist(scaled_nci_data), method = "average")
nci_hc_single = hclust(dist(scaled_nci_data), method = "single")
#3
hc_clusters = cutree(nci_hc_complete, 4)
table(nci_labs, hc_clusters)

hc_clusters = cutree(nci_hc_average, 4)
table(nci_labs, hc_clusters)

hc_clusters = cutree(nci_hc_single, 4)
table(nci_labs, hc_clusters)
#4
set.seed(123)
km_out = kmeans(scaled_nci_data, 4, nstart = 50)
km_clusters = km_out$cluster
table(nci_labs, km_clusters)
#5
set.seed(123)
table(km_clusters, hc_clusters)
library(mclust)
m3 <- Mclust(scaled_nci_data, G=4)
m3$classification
table(m3$classification, hc_clusters)
