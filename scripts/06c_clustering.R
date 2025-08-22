# Extract only the scaled columns for clustering
clustering_matrix <- trust_scaled %>%
  select(scaled_18, scaled_52) %>%
  as.matrix()

# Perform hierarchical clustering (euclidean + complete linkage)
dist_matrix <- dist(clustering_matrix, method = "euclidean")
hclust_model <- hclust(dist_matrix, method = "complete")

# Plot dendrogram
plot(hclust_model, labels = FALSE, main = "Dendrogram of NHS Trusts", xlab = "", sub = "", cex = 0.6)

# Cut into 4 clusters (change number if needed)
trust_scaled$cluster <- cutree(hclust_model, k = 5)

# Save clustered dataset
write_csv(trust_scaled, "output/clustering/trust_clusters.csv")

