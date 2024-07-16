library(terra)
library(ggplot2)

# Function to read NDVI images
read_ndvi_images <- function(path, start_year, end_year) {
  file_list <- list.files(path, pattern = "predicted_ndvi_.*\\.tif$", full.names = TRUE)
  file_list <- file_list[grepl(paste0(start_year:end_year, collapse = "|"), file_list)]
  ndvi_stack <- rast(file_list)
  return(ndvi_stack)
}

# Path to NDVI images
ndvi_path <- "C:/Users/Jado/Documents/EAGLE/Semester2/Cloud_computing/predicted_rasters"
ndvi_stack <- read_ndvi_images(ndvi_path, 1982, 2021)

# Function to create yearly composites by selecting the mean NDVI
create_yearly_composite <- function(ndvi_stack, start_year, end_year) {
  yearly_composites <- list()
  for (year in start_year:end_year) {
    year_indices <- which(grepl(paste0("_", year), names(ndvi_stack)))
    yearly_ndvi <- ndvi_stack[[year_indices]]
    yearly_composite <- app(yearly_ndvi, mean, na.rm = TRUE)
    yearly_composites[[as.character(year)]] <- yearly_composite
  }
  return(yearly_composites)
}

# Now, create yearly composites for the entire time period
start_year <- 1982
end_year <- 2021
yearly_composites <- create_yearly_composite(ndvi_stack, start_year, end_year)

# Function to classify NDVI image using a threshold
classify_ndvi_image <- function(ndvi_image, threshold = 0.6) {
  classified_image <- ndvi_image >= threshold
  classified_image <- classify(classified_image, cbind(1, 2), classes = c("Nonforest", "Forest"))
  return(classified_image)
}

# Classify yearly composites using the threshold
classify_yearly_composites <- function(yearly_composites, output_path) {
  classified_images <- list()
  for (year in names(yearly_composites)) {
    ndvi_image <- yearly_composites[[year]]
    
    # Classify NDVI image for the current year
    classified_image <- classify_ndvi_image(ndvi_image)
    classified_images[[year]] <- classified_image
    
    # Save the classified image to drive (on the computer)
    writeRaster(classified_image, 
                filename = paste0(output_path, "/classified_", year, ".tif"), 
                filetype = "GTiff", overwrite = TRUE)
  }
  return(classified_images)
}

# Define output path to save classified images
output_path <- "C:/Users/Jado/Documents/EAGLE/Semester3/Other_tasks/Binary_classification/Classified_Yearly_Forest_Vs_NonForest"

classified_images <- classify_yearly_composites(yearly_composites, output_path)

# Function to calculate accuracy
calculate_accuracy <- function(classified_image, ndvi_composite, threshold = 0.6) {
  true_labels <- classify(ndvi_composite >= threshold, cbind(0, 1), classes = c("Nonforest", "Forest"))
  predicted_labels <- classified_image
  confusion_matrix <- table(True = values(true_labels), Predicted = values(predicted_labels))
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  return(list(confusion_matrix = confusion_matrix, accuracy = accuracy))
}

# Calculate accuracy for each year
accuracy_list <- lapply(names(classified_images), function(year) {
  classified_image <- classified_images[[year]]
  ndvi_composite <- yearly_composites[[year]]
  accuracy_data <- calculate_accuracy(classified_image, ndvi_composite)
  return(c(year, accuracy_data$accuracy))
})

# Convert to data frame
accuracy_df <- data.frame(matrix(unlist(accuracy_list), nrow = length(accuracy_list), byrow = TRUE))
colnames(accuracy_df) <- c("Year", "Accuracy")

# Print accuracy for each year
print(accuracy_df)

# Plot accuracy over time
ggplot(accuracy_df, aes(x = as.numeric(as.character(Year)), y = as.numeric(as.character(Accuracy)))) +
  geom_line() +
  labs(x = "Year", y = "Accuracy", title = "Classification Accuracy over time") +
  theme_minimal()
