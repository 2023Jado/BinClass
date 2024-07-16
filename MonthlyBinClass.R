library(terra)
library(ggplot2)

# Function to read NDVI images
read_ndvi_images <- function(path) {
  file_list <- list.files(path, pattern = "predicted_ndvi_.*\\.tif$", full.names = TRUE)
  ndvi_images <- lapply(file_list, rast)
  names(ndvi_images) <- file_list
  return(ndvi_images)
}

# Path to NDVI images
ndvi_path <- "C:/Users/Jado/Documents/EAGLE/Semester2/Cloud_computing/predicted_rasters"

# Read NDVI images
ndvi_images <- read_ndvi_images(ndvi_path)

# Function to classify NDVI image using a threshold
classify_ndvi_image <- function(ndvi_image, threshold = 0.6) {
  classified_image <- ndvi_image >= threshold
  classified_image <- classify(classified_image, cbind(0, 1), classes = c("Nonforest", "Forest"))
  return(classified_image)
}

# Classify NDVI images
classified_images <- lapply(ndvi_images, classify_ndvi_image)
names(classified_images) <- names(ndvi_images)

# Define output path to save classified images
output_path <- "C:/Users/Jado/Documents/EAGLE/Semester3/Other_tasks/Binary_classification/Classified_Monthly_Forest_Vs_NonForest"

# Function to save classified images
save_classified_images <- function(classified_images, output_path) {
  for (file in names(classified_images)) {
    classified_image <- classified_images[[file]]
    output_file <- gsub("predicted_ndvi_", "classified_", file)
    output_file <- file.path(output_path, basename(output_file))
    writeRaster(classified_image, filename = output_file, filetype = "GTiff", overwrite = TRUE)
  }
}

# Save classified images
save_classified_images(classified_images, output_path)

# Function to calculate accuracy
calculate_accuracy <- function(classified_image, ndvi_image, threshold = 0.6) {
  true_labels <- classify(ndvi_image >= threshold, cbind(0, 1), classes = c("Nonforest", "Forest"))
  predicted_labels <- classified_image
  confusion_matrix <- table(True = values(true_labels), Predicted = values(predicted_labels))
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  return(list(confusion_matrix = confusion_matrix, accuracy = accuracy))
}

# Calculate accuracy for each classified image
accuracy_list <- lapply(names(classified_images), function(file) {
  classified_image <- classified_images[[file]]
  ndvi_image <- ndvi_images[[file]]
  accuracy_data <- calculate_accuracy(classified_image, ndvi_image)
  return(c(basename(file), accuracy_data$accuracy))
})

# Convert to data frame
accuracy_df <- data.frame(matrix(unlist(accuracy_list), nrow = length(accuracy_list), byrow = TRUE))
colnames(accuracy_df) <- c("File", "Accuracy")

# Print accuracy for each image
print(accuracy_df)

# Plot accuracy over time
accuracy_df$Date <- as.Date(sub("predicted_ndvi_", "", sub("\\.tif$", "", accuracy_df$File)), format = "%Y_%m")
ggplot(accuracy_df, aes(x = Date, y = as.numeric(as.character(Accuracy)))) +
  geom_line() +
  labs(x = "Date", y = "Accuracy", title = "Classification Accuracy over Time") +
  theme_minimal()
