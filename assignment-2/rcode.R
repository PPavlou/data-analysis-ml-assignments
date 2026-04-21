# Read Excel files
library(readxl)

# Data manipulation
library(dplyr)

# Clustering tools (silhouette, clusGap, etc.)
library(cluster)

# Visualization of clustering, Elbow, Silhouette, Gap
library(factoextra)

# General plotting
library(ggplot2)


# Read the Excel file with hotel bookings
df <- read_excel(file.choose())  

# Basic information: number of rows and columns
dim(df)

# Structure of the dataset (types of variables)
str(df)

# Number of missing values per column
colSums(is.na(df))

# Basic descriptive statistics for key numeric-related variables
summary(df[, c("lead.time",
               "average.price",
               "number.of.adults",
               "number.of.children",
               "number.of.weekend.nights",
               "number.of.week.nights",
               "special.requests")])


# Drop variables that are not used anywhere in the analysis
# (Booking ID, meal type, parking, room type)
df <- df %>%
  select(
    -Booking_ID,
    -type.of.meal,
    -car.parking.space,
    -room.type
  )

# Convert variables to appropriate types (factor / numeric)
df <- df %>%
  mutate(
    # Categorical variables
    market.segment.type = as.factor(market.segment.type),
    booking.status      = as.factor(booking.status),
    repeated            = as.factor(repeated),
    
    # Average price as numeric
    average.price       = as.numeric(average.price)
  )

str(df)


df <- df %>%
  mutate(
    # Total number of guests (adults + children)
    persons = number.of.adults + number.of.children,
    
    # Total number of nights (weekend + weekdays)
    nights  = number.of.weekend.nights + number.of.week.nights,
    
    # Total number of previous bookings
    prev_total       = P.C + P.not.C,
    
    # Indicator of having any previous booking
    has_prev_booking = ifelse(prev_total > 0, 1, 0),
    
    # Indicator of having any previous cancellation
    has_prev_cancel  = ifelse(P.C > 0, 1, 0)
  )

# Check basic summary for the new derived variables
summary(df[, c("persons", "nights", "prev_total", "has_prev_booking", "has_prev_cancel")])


# Helper function: compute mean silhouette for a given set of variables
compute_sil_for_vars <- function(df_in, vars, k_min = 2, k_max = 8) {
  # Keep only selected variables
  df_sub <- df_in[, vars]
  
  # Remove potential missing values (for safety)
  df_sub <- na.omit(df_sub)
  
  # Standardize variables (mean 0, sd 1)
  X <- scale(df_sub)
  
  ks <- k_min:k_max
  
  # Compute mean silhouette for each k
  sils <- sapply(ks, function(k) {
    set.seed(123)
    km  <- kmeans(X, centers = k, nstart = 25)
    sil <- silhouette(km$cluster, dist(X))
    mean(sil[, 3])
  })
  
  data.frame(k = ks, mean_sil = sils)
}

# Case 1: Base behavioural variables only
base_vars <- c(
  "persons",
  "nights",
  "lead.time",
  "average.price"
)

sil_base <- compute_sil_for_vars(df, base_vars, k_min = 2, k_max = 8)
sil_base  # Mean silhouette for different k using 4 base variables

# Case 2: Final set of 6 variables used for clustering
final_vars <- c(
  "persons",
  "nights",
  "lead.time",
  "average.price",
  "has_prev_booking",
  "has_prev_cancel"
)

sil_final <- compute_sil_for_vars(df, final_vars, k_min = 2, k_max = 8)
sil_final  # Mean silhouette for different k using 6 variables


# Keep only the six final variables for clustering
df_num <- df[, final_vars]

# Standardize features (mean 0, sd 1)
df_scaled <- scale(df_num)

# Distance matrix on standardized data (used by both methods)
dist_mat <- dist(df_scaled)


set.seed(123)  # Reproducibility

# Elbow plot: WSS vs number of clusters
fviz_nbclust(
  df_scaled,
  kmeans,
  method = "wss"
) +
  ggtitle("Elbow Plot (WSS vs. Number of Clusters)") +
  xlab("Number of clusters (k)") +
  ylab("Total within-cluster sum of squares (WSS)") +
  theme_gray()


set.seed(123)  # Reproducibility

# Define the range of k
k_min <- 2
k_max <- 8
ks    <- k_min:k_max

# Compute mean silhouette for each k using the same data (df_scaled, dist_mat)
sil_vals <- sapply(ks, function(k) {
  km  <- kmeans(df_scaled, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist_mat)
  mean(sil[, 3])
})

# Put results in a data frame for plotting and inspection
df_sil_plot <- data.frame(
  k        = ks,
  mean_sil = sil_vals
)

# Print the values to check (should match the ones you report in the text)
df_sil_plot

# Plot mean silhouette vs k
ggplot(df_sil_plot, aes(x = k, y = mean_sil)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Mean Silhouette Index vs. Number of Clusters",
    x     = "Number of clusters (k)",
    y     = "Mean silhouette index"
  ) +
  theme_gray()


set.seed(123)  # Reproducibility

# Compute Gap statistic for k-means
gap_stat <- clusGap(
  df_scaled,
  FUN    = kmeans,
  nstart = 25,
  K.max  = 8,
  B      = 50
)

# Plot Gap statistic
fviz_gap_stat(gap_stat) +
  ggtitle("Gap Statistic for k-means") +
  xlab("Number of clusters (k)") +
  ylab("Gap statistic") +
  theme_gray()


set.seed(123)  # Reproducibility

# Run k-means with k = 2 on standardized data
km2 <- kmeans(df_scaled, centers = 2, nstart = 25)

# Attach k-means cluster labels to the main data frame
df$cluster_kmeans <- factor(km2$cluster)

# Cluster sizes for k-means
table(df$cluster_kmeans)

# Silhouette analysis for k-means solution
sil_km2  <- silhouette(as.integer(df$cluster_kmeans), dist_mat)
km_sil   <- summary(sil_km2)$avg.width
km_sil   # Mean silhouette for k-means with k = 2

# Visualization of k-means clusters in PCA space
fviz_cluster(
  km2,
  data            = df_scaled,
  geom            = "point",
  ellipse.type    = "norm",
  show.clust.cent = TRUE,
  main            = "k-means clustering (k = 2)"
)

# Cluster profiles (original scale) for k-means
km_profiles <- df %>%
  group_by(cluster_kmeans) %>%
  summarise(
    n                = n(),
    mean_persons     = mean(persons),
    mean_nights      = mean(nights),
    mean_lead        = mean(lead.time),
    mean_price       = mean(average.price),
    mean_prev_book   = mean(has_prev_booking),
    mean_prev_cancel = mean(has_prev_cancel)
  )

km_profiles

# Distribution of booking status within k-means clusters
km_cancel <- df %>%
  group_by(cluster_kmeans, booking.status) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cluster_kmeans) %>%
  mutate(prop = n / sum(n))

km_cancel

# Plot cancellation rates by k-means cluster
ggplot(km_cancel,
       aes(x = cluster_kmeans, y = prop, fill = booking.status)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Cancellation rates by cluster – k-means (k = 2)",
    x     = "Cluster (k-means)",
    y     = "Proportion within cluster",
    fill  = "Booking status"
  ) +
  theme_gray()


# Hierarchical clustering with Ward's method on the same distance matrix
hc <- hclust(dist_mat, method = "ward.D2")

# Dendrogram (full tree)
plot(hc,
     labels = FALSE,
     main   = "Hierarchical clustering dendrogram (Ward.D2)",
     xlab   = "",
     sub    = "")

# Highlight the cut for k = 2 clusters
rect.hclust(hc, k = 2, border = "red")

# Cut dendrogram into 2 clusters
hc_clusters2 <- cutree(hc, k = 2)

# Attach hierarchical cluster labels
df$cluster_hclust <- factor(hc_clusters2)

# Cluster sizes for hierarchical clustering
table(df$cluster_hclust)

# Silhouette analysis for hierarchical solution
sil_hc2  <- silhouette(hc_clusters2, dist_mat)
hc_sil   <- summary(sil_hc2)$avg.width
hc_sil   # Mean silhouette for hierarchical clustering with k = 2

# Cluster profiles (original scale) for hierarchical clustering
hc_profiles <- df %>%
  group_by(cluster_hclust) %>%
  summarise(
    n                = n(),
    mean_persons     = mean(persons),
    mean_nights      = mean(nights),
    mean_lead        = mean(lead.time),
    mean_price       = mean(average.price),
    mean_prev_book   = mean(has_prev_booking),
    mean_prev_cancel = mean(has_prev_cancel)
  )

hc_profiles

# Distribution of booking status within hierarchical clusters
hc_cancel <- df %>%
  group_by(cluster_hclust, booking.status) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cluster_hclust) %>%
  mutate(prop = n / sum(n))

hc_cancel

# Plot cancellation rates by hierarchical cluster
ggplot(hc_cancel,
       aes(x = cluster_hclust, y = prop, fill = booking.status)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Cancellation rates by cluster – Hierarchical (Ward, k = 2)",
    x     = "Cluster (hierarchical)",
    y     = "Proportion within cluster",
    fill  = "Booking status"
  ) +
  theme_gray()


# Cross-tabulation of cluster assignments
table_km_vs_hc <- table(
  kmeans = df$cluster_kmeans,
  hclust = df$cluster_hclust
)

table_km_vs_hc  # Agreement between the two methods

# The silhouette values km_sil and hc_sil can be reported in the report
km_sil
hc_sil

