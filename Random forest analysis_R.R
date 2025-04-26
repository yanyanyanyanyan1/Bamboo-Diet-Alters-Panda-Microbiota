# Load required packages
library(randomForest)
library(ggplot2)
library(tidyr)
library(caret)
library(dplyr)

# Read data (replace with your actual file path)
data_file <- "your_data_file.csv"  # Input file path
data <- read.csv(data_file, header = TRUE)

# Extract filename (without path and extension)
file_name <- tools::file_path_sans_ext(basename(data_file))

# Convert Group to factor
data$Group <- as.factor(data$Group)

# Initialize storage for variable importance and accuracy
variable_names <- colnames(data)[colnames(data) != "Group"]
importance_matrix <- matrix(0, nrow = 100, ncol = length(variable_names))
colnames(importance_matrix) <- variable_names
accuracy_vector <- numeric(100)  # Store accuracy for each run

# Tune mtry parameter
tuneGrid <- expand.grid(mtry = c(20,30,40,50,60,70,80,90,100))  # Adjust range based on feature count
control <- trainControl(method = "LOOCV", number = 10)  # 10-fold cross-validation
rf_tune <- train(Group ~ ., 
                data = data, 
                method = "rf", 
                tuneGrid = tuneGrid, 
                ntree = 4000, 
                trControl = control)
print(rf_tune)

# Determine optimal ntree
rf_model <- randomForest(Group ~ ., 
                        mtry = 20, 
                        data = data, 
                        ntree = 5000)
error_rates <- rf_model$err.rate
plot(1:5000, error_rates[, 1], 
     type = "l", 
     xlab = "Number of Trees", 
     ylab = "Error Rate")

# Run random forest 100 times
for (i in 1:100) {
  # Run random forest
  rf_model <- randomForest(Group ~ ., 
                          data, 
                          mtry = 3, 
                          ntree = 5000, 
                          importance = TRUE)
  
  # Get variable importance
  importance_values <- importance(rf_model, type = 1)[, 1]  # Mean Decrease Accuracy
  importance_matrix[i, ] <- importance_values
  
  # Calculate prediction accuracy
  predictions <- predict(rf_model)
  accuracy_vector[i] <- mean(predictions == data$Group)
}

# Calculate mean variable importance and accuracy
mean_importance <- colMeans(importance_matrix)
average_accuracy <- mean(accuracy_vector)

# Print average accuracy
print(paste("Average accuracy across 100 runs: ", 
            round(average_accuracy * 100, 2), "%", sep = ""))

# Prepare importance data for plotting
importance_df <- data.frame(
  Variable = names(mean_importance),
  Importance = mean_importance
)

# Select top 10 important variables
top10_importance_df <- importance_df %>%
  arrange(desc(Importance)) %>%
  slice(1:10)

# Plot top 10 variables
pdf(paste0(file_name, "_top10_importance_dotplot.pdf"), height = 6, width = 8)
ggplot(top10_importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_point(color = "#1f77b4", size = 4, shape = 1) +
  coord_flip() +
  labs(title = paste("Top 10 Important Variables (100 runs)\nAccuracy: ", 
                     round(average_accuracy * 100, 2), "%", sep = ""), 
       x = "Variables", 
       y = "Mean Decrease Accuracy") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", color = "#333333"), 
    axis.title = element_text(size = 12, face = "bold", color = "#333333"),
    axis.text = element_text(size = 10, color = "#333333"),
    panel.border = element_rect(color = "black", fill = NA, size = 1), 
    panel.grid = element_blank(),   
    axis.line = element_line(color = "black"),   
    axis.ticks = element_line(color = "black"), 
    axis.ticks.length = unit(0.2, "cm"),         
    panel.grid.major.y = element_line(color = "grey80", linetype = "dashed", size = 0.5), 
    aspect.ratio = 1.5,
    plot.background = element_rect(fill = "white", color = NA), 
    axis.title.y = element_text(face = "italic", size = 12, color = "#333333"), 
    axis.text.y = element_text(face = "italic", size = 10, color = "#333333")
  )
dev.off()

# Export variable importance to CSV
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]
write.csv(importance_df, 
          paste0(file_name, "_mean_importance.csv"), 
          row.names = FALSE)