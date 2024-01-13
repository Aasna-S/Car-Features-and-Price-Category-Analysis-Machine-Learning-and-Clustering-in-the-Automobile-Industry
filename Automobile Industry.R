library(randomForest) 
library(cluster)      
library(ggplot2)    
library(MASS)         
library(e1071)    
library(MASS)
library(car)
install.packages("corrplot")
library(corrplot)
library(reshape2)


# Import dataset
car_data <- read.csv("C:/Users/Aasna/Downloads/Dataset 5 — Automobile data.csv")

#EDA + Preprocessing 
summary(car_data)
str(car_data)


# Check for missing values
missing_values <- sapply(car_data, function(x) sum(is.na(x)))
print(missing_values)

# Find the number of records with '?' value for normalized losses
car_data$normalized.losses <- as.character(car_data$normalized.losses)
count_missing <- sum(car_data$normalized.losses == '?')
print(count_missing)

nl <- as.numeric(car_data$normalized.losses[car_data$normalized.losses != '?'])
nl_mean <- mean(nl, na.rm = TRUE)
car_data$normalized.losses[car_data$normalized.losses == '?'] <- nl_mean
car_data$normalized.losses <- as.numeric(car_data$normalized.losses)

car_data$price <- as.character(car_data$price)
count_non_numeric <- sum(!grepl("^[0-9]+$", car_data$price))
print(count_non_numeric)

# List out the values which are not numeric
non_numeric_values <- car_data$price[!grepl("^[0-9]+$", car_data$price)]
print(non_numeric_values)

# Setting the missing value to the mean of price and converting the datatype to numeric
price <- as.numeric(car_data$price[grepl("^[0-9]+$", car_data$price)])
p_mean <- mean(price, na.rm = TRUE)
car_data$price[!grepl("^[0-9]+$", car_data$price)] <- p_mean
car_data$price <- as.numeric(car_data$price)

# Handling 'horsepower' column
car_data$horsepower <- as.character(car_data$horsepower)
is_numeric <- grepl("^[0-9]+$", car_data$horsepower)
horsepower <- as.numeric(car_data$horsepower[is_numeric])
hp_mean <- mean(horsepower, na.rm = TRUE)
car_data$horsepower[!is_numeric] <- hp_mean
car_data$horsepower <- as.numeric(car_data$horsepower)

# Handling 'bore' column
car_data$bore <- as.character(car_data$bore)
invalid_bore <- sum(car_data$bore == '?')
print(paste("Number of invalid values in 'bore':", invalid_bore))
car_data$bore[car_data$bore == '?'] <- NA
car_data$bore <- as.numeric(car_data$bore)

# Handling 'stroke' column
car_data$stroke <- as.character(car_data$stroke)
car_data$stroke[car_data$stroke == '?'] <- NA
car_data$stroke <- as.numeric(car_data$stroke)

# Handling 'peak.rpm' column
car_data$peak.rpm <- as.character(car_data$peak.rpm)
car_data$peak.rpm[car_data$peak.rpm == '?'] <- NA
car_data$peak.rpm <- as.numeric(car_data$peak.rpm)

# Handling 'num.of.doors' column
car_data$num.of.doors <- as.character(car_data$num.of.doors)
car_data <- car_data[car_data$num.of.doors != '?', ]
car_data$num.of.doors <- as.factor(car_data$num.of.doors)

# Convert other columns to their appropriate types
car_data$symboling <- as.integer(car_data$symboling)
car_data$make <- as.character(car_data$make)
car_data$fuel.type <- as.character(car_data$fuel.type)
car_data$aspiration <- as.character(car_data$aspiration)
car_data$num.of.doors <- as.character(car_data$num.of.doors)
car_data$body.style <- as.character(car_data$body.style)
car_data$drive.wheels <- as.character(car_data$drive.wheels)
car_data$engine.location <- as.character(car_data$engine.location)
car_data$engine.type <- as.character(car_data$engine.type)
car_data$num.of.cylinders <- as.character(car_data$num.of.cylinders) 
car_data$fuel.system <- as.character(car_data$fuel.system)


#Calculate Skewness
numeric_variables <- c("wheel.base", "length", "width", "curb.weight", "engine.size", "bore", "stroke", "compression.ratio", "horsepower", "peak.rpm", "city.mpg", "highway.mpg", "price")

# Loop through each numeric variable to ensure it's numeric and calculate skewness
for (var in numeric_variables) {
  # Convert the column to numeric, handling NAs and non-numeric placeholders
  car_data[[var]] <- as.numeric(replace(car_data[[var]], car_data[[var]] == "?", NA))
  
  # Calculate skewness, removing NA values
  skewness_value <- skewness(car_data[[var]], na.rm = TRUE)
  
  # Print the skewness value
  print(paste("Skewness for", var, ":", skewness_value))
}


#Histogram + Boxplots

for (var in numeric_variables) {
  # Ensure the variable is numeric and replace any non-numeric placeholders with NA
  car_data[[var]] <- as.numeric(replace(car_data[[var]], car_data[[var]] == "?", NA))
  
  # Set up the plotting area to have 1 row and 2 columns
  par(mfrow = c(1, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
  
  # Create a histogram for the variable, excluding NA values
  hist(car_data[[var]], 
       main = paste("Histogram of", var), 
       xlab = var, 
       na.rm = TRUE, 
       col = "#6baed6", # Set the color of the bars to a shade of blue
       border = "#3182bd", # Set the color of the border of the bars to a darker shade of blue
       las = 1, # Orient axis labels horizontally
       cex.lab = 1, # Adjust axis labels size
       cex.main = 1, # Adjust the main title size
       cex.axis = 1) # Adjust axis text size
  
  # Create a boxplot for the variable, excluding NA values
  boxplot(car_data[[var]], 
          main = paste("Boxplot of", var), 
          xlab = var, 
          na.rm = TRUE, 
          col = "#9ecae1", # Set the color of the boxes to a different shade of blue
          las = 1, # Orient axis labels horizontally
          cex.lab = 1, # Adjust axis labels size
          cex.main = 1, # Adjust the main title size
          cex.axis = 1, # Adjust axis text size
          notch = FALSE, # Remove notches if preferred
          outline = TRUE) # Include outliers
}

# Reset the plot layout to default
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))

#Outliers 

for (var in numeric_variables) {
  Q1 <- quantile(car_data[[var]], 0.25, na.rm = TRUE)
  Q3 <- quantile(car_data[[var]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- car_data[[var]][car_data[[var]] < lower_bound | car_data[[var]] > upper_bound]
  cat(paste("Outliers for", var, ":", length(outliers), "\n"))
  
}

# Calculate the IQR and bounds for the "price" variable
Q1_price <- quantile(car_data$price, 0.25, na.rm = TRUE)
Q3_price <- quantile(car_data$price, 0.75, na.rm = TRUE)
IQR_price <- Q3_price - Q1_price
lower_bound_price <- Q1_price - 1.5 * IQR_price
upper_bound_price <- Q3_price + 1.5 * IQR_price

outliers_price <- car_data$price[car_data$price < lower_bound_price | car_data$price > upper_bound_price]


cat(paste("Outliers for price:", length(outliers_price), "\n"))

#remove outliers 

#COLINEARITY 

lm_model <- lm(price ~ ., data = car_data[numeric_variables])

# calculate VIF
vif_values <- vif(lm_model)
print(vif_values)

#Correlation  Analysis 

#PRICE + OTHER VARS
car_data[numeric_variables] <- lapply(car_data[numeric_variables], function(x) as.numeric(as.character(x)))

# Calculate the correlation matrix
cor_matrix <- cor(car_data[numeric_variables], use = "complete.obs")  # use "complete.obs" to handle missing values

print(cor_matrix)
# Extract correlations of each variable with 'price'
price_correlations <- cor_matrix["price", ]
print(price_correlations)

# Create a correlation matrix for price correlations
price_correlations <- cor(car_data[numeric_variables], use = "complete.obs")["price", ]

# Assuming you already have the price_correlations vector from your previous code
# Convert the price_correlations vector into a matrix
price_cor_matrix <- matrix(price_correlations, nrow = 1)


# Create a correlation plot for the price correlations matrix
corrplot(
  price_cor_matrix,       # Your correlation matrix for price
  method = "color",       # Specify the method for displaying correlations
  type = "upper",         # Show only the upper triangle of the correlation matrix
  tl.col = "black",       # Color of text labels
  tl.srt = 45,            # Rotate text labels by 45 degrees
  diag = FALSE,           # Exclude diagonal elements
  addCoef.col = "black",  # Color of coefficient values
  order = "hclust",       # Reorder variables using hierarchical clustering
  p.mat = NULL,           # P-values for correlations (set to NULL as you're not using them)
  sig.level = 0.01,       # Significance level for coloring (adjust as needed)
  insig = "blank"         # Display non-significant correlations as blank
)

# Calculate the correlation matrix
cor_matrix <- cor(car_data[numeric_variables], use = "complete.obs")


# Create a correlation plot for the full correlation matrix
corrplot(
  cor_matrix,             # Your full correlation matrix
  method = "color",       # Method for displaying correlations
  type = "upper",         # Show only the upper triangle of the matrix
  tl.col = "black",       # Text label color
  tl.srt = 45,            # Rotate text labels by 45 degrees
  addCoef.col = "black",  # Color of coefficient values
  order = "hclust",       # Reorder variables using hierarchical clustering
  diag = FALSE            # Exclude diagonal elements
)


#RANDOM FOREST MODEL 


quantiles <- quantile(car_data$price, probs = c(0.33, 0.66), na.rm = TRUE)
car_data$price_category <- cut(car_data$price,
                               breaks = c(-Inf, quantiles, Inf),
                               labels = c("Low", "Medium", "High"),
                               include.lowest = TRUE)

categorical_vars <- c("make", "fuel.type", "aspiration", "num.of.doors", "body.style", 
                      "drive.wheels", "engine.location", "engine.type", "num.of.cylinders", 
                      "fuel.system")
car_data[categorical_vars] <- lapply(car_data[categorical_vars], factor)


set.seed(123)  # for reproducibility
train_indices <- sample(1:nrow(car_data), 0.8 * nrow(car_data))
train_data <- car_data[train_indices, ]
test_data <- car_data[-train_indices, ]
# Check for missing values in training data
sum(is.na(train_data))


numeric_columns <- sapply(train_data, is.numeric)
train_data[numeric_columns] <- lapply(train_data[numeric_columns], function(x) replace(x, is.na(x), median(x, na.rm = TRUE)))

categorical_columns <- sapply(train_data, is.factor)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

train_data[categorical_columns] <- lapply(train_data[categorical_columns], function(x) replace(x, is.na(x), get_mode(x)))

rf_model <- randomForest(price_category ~ . - price, data = train_data, ntree = 500)

rf_predictions <- predict(rf_model, test_data)
confusion_matrix <- table(test_data$price_category, rf_predictions)
print(confusion_matrix)



accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))


# Function to calculate precision for a given class
precision <- function(conf_matrix, class) {
  tp <- conf_matrix[class, class]
  col_sum <- sum(conf_matrix[, class])
  if (col_sum == 0) return(0)
  tp / col_sum
}

# Function to calculate recall for a given class
recall <- function(conf_matrix, class) {
  tp <- conf_matrix[class, class]
  row_sum <- sum(conf_matrix[class, ])
  if (row_sum == 0) return(0)
  tp / row_sum
}

# Function to calculate F1-score
f1_score <- function(precision, recall) {
  if (precision + recall == 0) return(0)
  2 * (precision * recall) / (precision + recall)
}

# Apply the functions to each class
classes <- dimnames(confusion_matrix)[[1]]  # Class names
metrics <- sapply(classes, function(class) {
  prec <- precision(confusion_matrix, class)
  rec <- recall(confusion_matrix, class)
  f1 <- f1_score(prec, rec)
  c(Precision = prec, Recall = rec, `F1-Score` = f1)
})

print(metrics)

# Convert predicted and actual categories to numerical values
num_rf_predictions <- as.numeric(factor(rf_predictions, levels = c("Low", "Medium", "High")))
num_test_data <- as.numeric(factor(test_data$price_category, levels = c("Low", "Medium", "High")))

sum(is.na(num_rf_predictions))
sum(is.na(num_test_data))
# Calculate MSE
mse <- mean((num_test_data - num_rf_predictions)^2, na.rm = TRUE)
print(paste("MSE:", mse))


# Extract feature importance
feature_importance <- importance(rf_model)

# Convert to a data frame for plotting
feature_importance_df <- as.data.frame(feature_importance)
feature_importance_df$Feature <- rownames(feature_importance_df)

feature_importance_melted <- melt(feature_importance_df, id.vars = "Feature")

# Define shades of blue for the plot
blue_palette <- c("#4F81BD", "#6FADE4", "#8CB6DB", "#B0DAF1")

# Plotting feature importance with improved styling and no legend
ggplot(feature_importance_melted, aes(x = reorder(Feature, value), y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ variable, scales = "free") +
  scale_fill_manual(values = blue_palette) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"
  ) +
  xlab("Features") +
  ylab("Importance") +
  ggtitle("Feature Importance in Random Forest Model")


print(feature_importance)

#K-MEANS CLUSTERING 

# Selected features
selected_features <- c("curb.weight", "highway.mpg", "horsepower", "city.mpg", "length", "wheel.base", "width")

# Subset the data with selected features
selected_data <- car_data[, selected_features]
# Check for NA values and handle them if necessary
if(anyNA(selected_data)) {
  # Handle NA values; for example, replace them with the mean of the respective column
  for(i in seq_along(selected_data)) {
    selected_data[is.na(selected_data[, i]), i] <- mean(selected_data[, i], na.rm = TRUE)
  }
}


scaled_data <- scale(selected_data)

wcss <- vector('double', length = 10)   
max_clusters <- 10  

for (i in 1:max_clusters) {
  set.seed(123)
  kmeans_result <- kmeans(scaled_data, centers = i, nstart = 20)
  wcss[i] <- sum(kmeans_result$tot.withinss)
}

plot(1:max_clusters, wcss, type = 'b', pch = 19, col = "blue", 
     main = "Elbow Plot for Optimal K",
     xlab = "Number of Clusters (K)", ylab = "Within-Cluster Sum of Squares (WCSS)",
     xlim = c(1, max_clusters), ylim = c(0, max(wcss) * 1.1))
  
abline(h = wcss[1], col = "red", lty = 2)  


set.seed(123) 
k <- 3
kmeans_result <- kmeans(scaled_data, centers = k, nstart = 20)


car_data$cluster <- kmeans_result$cluster

print(table(car_data$cluster))


ggplot(car_data, aes(x = factor(cluster), fill = factor(cluster))) +
  geom_bar(show.legend = FALSE) +  # Remove legend if not necessary
  scale_fill_manual(values = c("lightblue", "blue", "darkblue")) +  # Custom color palette
  labs(title = "Cluster Distribution",
       subtitle = "Distribution of Cars across Different Clusters",
       x = "Cluster",
       y = "Count") +
  theme_minimal() +  # Use a minimal theme
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center and bold the title
        plot.subtitle = element_text(hjust = 0.5, size = 12),  # Center the subtitle
        axis.title = element_text(size = 12, face = "bold"),  # Bold axis titles
        axis.text = element_text(size = 10),  # Adjust axis text size
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_rect(fill = "white", colour = "white"))  # White background


pca_data <- prcomp(scaled_data, scale = TRUE)

pca_df <- as.data.frame(pca_data$x)

pca_df$cluster <- factor(kmeans_result$cluster)

fviz_cluster(list(data = pca_df[, 1:2], cluster = pca_df$cluster), 
             geom = "point", 
             ellipse.type = "convex", 
             ggtheme = theme_minimal()) +
  ggtitle("K-Means Clustering") +
  theme(plot.title = element_text(hjust = 0.5))  

cluster_centroids <- aggregate(selected_data[, -ncol(selected_data)], 
                               by=list(Cluster=kmeans_result$cluster), 
                               FUN=mean)

ggplot(cluster_centroids_melted, aes(x = factor(Cluster), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ variable, scales = "free") +
  labs(x = "Cluster", y = "Feature Value", title = "Cluster Profiles by Feature") +
  scale_fill_brewer(palette = "Blues") +  # Use a predefined blue palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title = element_text(face = "bold"),legend.position = "none",  # Remove the legend plot.title = element_text(hjust = 0.5)  # Center the title
  )

cluster_sizes <- table(kmeans_result$cluster)
print(cluster_sizes)


############PRICE CATEGORY + CLUSTERS##############

car_data$cluster <- kmeans_result$cluster


quantiles <- quantile(car_data$price, probs = c(0.33, 0.66), na.rm = TRUE)
car_data$price_category <- cut(car_data$price,
                               breaks = c(-Inf, quantiles, Inf),
                               labels = c("Low", "Medium", "High"),
                               include.lowest = TRUE)



cluster_price_table <- table(car_data$cluster, car_data$price_category)

print(cluster_price_table)


ggplot(car_data, aes(x = factor(cluster), fill = price_category)) +
  geom_bar(position = "dodge", show.legend = TRUE) +  # Enable legend
  scale_fill_brewer(palette = "Blues", name = "Price Category") +  # Use a blue color palette
  labs(title = "Distribution of Price Categories within Clusters",
       x = "Cluster",
       y = "Count") +
  theme_minimal(base_size = 14) +  # Apply a minimal theme with a base font size
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center and bold the title
        plot.subtitle = element_text(hjust = 0.5, size = 12),  # Center the subtitle
        axis.title = element_text(face = "bold"),  # Bold axis titles
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = "bottom",  # Place the legend at the bottom
        legend.title = element_text(face = "bold"),  # Bold the legend title
        legend.text = element_text(size = 10),  # Adjust legend text size
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_rect(fill = "white", colour = "white"))  # Set a white background



selected_features <- c("curb.weight", "highway.mpg", "horsepower", "city.mpg", "length", "wheel.base", "width")

cluster_data <- car_data[, selected_features]
cluster_data$cluster <- car_data$cluster  
cluster_data <- data.frame(lapply(cluster_data, function(x) as.numeric(as.character(x))))

for(i in seq_along(cluster_data)) {
  column_data <- cluster_data[, i]
  cluster_data[is.na(column_data), i] <- mean(column_data, na.rm = TRUE)
}

cluster_means <- aggregate(. ~ cluster, data = cluster_data, FUN = mean)

print(cluster_means)



melted_data <- melt(cluster_means, id.vars = 'cluster')

ggplot(melted_data, aes(x = variable, y = value, fill = factor(cluster))) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_minimal() +
  labs(x = 'Feature', y = 'Mean Value', fill = 'Cluster')


contingency_table <- table(car_data$cluster, car_data$price_category)

print(contingency_table)

chisq_test <- chisq.test(contingency_table)

print(chisq_test)

numeric_data <- car_data[, c('curb.weight', 'highway.mpg', 'horsepower', 'city.mpg', 'length', 'wheel.base', 'width')]


scaled_data <- scale(numeric_data)

pca_result <- prcomp(scaled_data)

pca_data <- data.frame(pca_result$x, cluster = car_data$cluster, price_category = car_data$price_category)
library(ggplot2)

ggplot(pca_data, aes(x = PC1, y = PC2, color = price_category, shape = factor(cluster))) +
  geom_point(size = 3, alpha = 0.7) +  # Adjust point size and transparency
  scale_color_brewer(type = "qual", palette = "Set1") +  # Use a qualitative color palette
  theme_minimal(base_size = 14) +  # Use a minimal theme with larger base font size
  theme(legend.position = "right",  # Adjust legend position
        legend.title.align = 0.5,  # Center align legend titles
        plot.title = element_text(hjust = 0.5),  # Center the plot title
        plot.subtitle = element_text(hjust = 0.5),  # Center the plot subtitle
        plot.caption = element_text(hjust = 0.5)) +  # Center the plot caption
  labs(title = "Principal Component Analysis",
       subtitle = "Projection of Cars Data",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Price Category",
       shape = "Cluster")

# Perform ANOVA for each feature across the price categories
results <- list()
for(feature in selected_features) {
  formula <- as.formula(paste(feature, "~ price_category"))
  anova_result <- aov(formula, data = car_data)
  summary_result <- summary(anova_result)
  results[[feature]] <- summary_result
}


print(results)

cluster_summary_stats <- aggregate(. ~ cluster, data = car_data, 
                                   FUN = function(x) c(mean = mean(x, na.rm = TRUE), 
                                                       sd = sd(x, na.rm = TRUE)))
names(cluster_summary_stats) <- gsub("\\.sd", ".SD", names(cluster_summary_stats))


print(cluster_summary_stats)






