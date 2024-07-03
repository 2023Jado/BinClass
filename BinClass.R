library(raster)
library(randomForest)
library(sp)
library(ggplot2)

# Function to read NDVI images
read_ndvi_images <- function(path, start_year, end_year) {
  file_list <- list.files(path, pattern = "predicted_ndvi_.*\\.tif$", full.names = TRUE)
  file_list <- file_list[grepl(paste0(start_year:end_year, collapse = "|"), file_list)]
  ndvi_stack <- stack(file_list)
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
    yearly_composite <- calc(yearly_ndvi, mean, na.rm = TRUE)
    yearly_composites[[as.character(year)]] <- yearly_composite
  }
  return(yearly_composites)
}

# Now, create yearly composites for the entire time period
start_year <- 1982
end_year <- 2021
yearly_composites <- create_yearly_composite(ndvi_stack, start_year, end_year)

# Calculate NDVI change between 1982 and 2021
ndvi_1982_composite <- yearly_composites[['1982']]
ndvi_2021_composite <- yearly_composites[['2021']]
ndvi_change <- ndvi_2021_composite - ndvi_1982_composite

# Find unchanged NDVI pixels (NDVI change close to zero)
threshold <- 0.01
unchanged_pixels <- which(abs(values(ndvi_change)) < threshold)

# Function to sample points from unchanged pixels
sample_points <- function(ndvi_composite, num_samples, unchanged_pixels) {
  sample_indices <- sample(unchanged_pixels, num_samples)
  sampled_points <- xyFromCell(ndvi_composite, sample_indices)
  return(sampled_points)
}

# Random sample points from unchanged pixels for training
set.seed(123) # for reproducibility
num_samples <- 12000
sampled_points <- sample_points(ndvi_2021_composite, num_samples, unchanged_pixels)

# Extract NDVI values for the sampled points in 2021
ndvi_2021_values <- extract(ndvi_2021_composite, sampled_points)

# Create a data frame with NDVI values and labels for training
training_data <- data.frame(
  x = coordinates(sampled_points)[, 1],
  y = coordinates(sampled_points)[, 2],
  NDVI = ndvi_2021_values,
  Class = ifelse(ndvi_2021_values >= 0.6, "Forest", "Nonforest")
)

# Convert Class to factor
training_data$Class <- as.factor(training_data$Class)

# Train Random Forest model
rf_model <- randomForest(Class ~ NDVI, data = training_data, ntree = 500)

# Function to classify NDVI image using trained model
classify_ndvi_image <- function(ndvi_image, rf_model) {
  # Extract NDVI values from the raster
  ndvi_values <- getValues(ndvi_image)
  
  # Create prediction data frame
  prediction_data <- data.frame(NDVI = ndvi_values)
  
  # Predict using the trained model
  prediction <- predict(rf_model, newdata = prediction_data)
  
  # Create a RasterLayer with the predictions
  classified_raster <- raster(ndvi_image)
  values(classified_raster) <- prediction
  
  return(classified_raster)
}

# Classify yearly composites using the trained model
classify_yearly_composites <- function(yearly_composites, rf_model, output_path) {
  classified_images <- list()
  for (year in names(yearly_composites)) {
    ndvi_image <- yearly_composites[[year]]
    
    # Classify NDVI image for the current year
    classified_image <- classify_ndvi_image(ndvi_image, rf_model)
    classified_images[[year]] <- classified_image
    
    # Save the classified image to drive (on the computer)
    writeRaster(classified_image, 
                filename = paste0(output_path, "/classified_", year, ".tif"), 
                format = "GTiff", overwrite = TRUE)
  }
  return(classified_images)
}

# Define output path to save classified images
output_path <- "C:/Users/Jado/Documents/EAGLE/Semester2/Cloud_computing/classified_images"

classified_images <- classify_yearly_composites(yearly_composites, rf_model, output_path)

# Function to calculate confusion matrix and accuracy
calculate_accuracy <- function(classified_image, ndvi_composite, num_samples, unchanged_pixels) {
  test_points <- sample_points(ndvi_composite, num_samples, unchanged_pixels)
  true_labels <- ifelse(extract(ndvi_composite, test_points) >= 0.6, "Forest", "Nonforest")
  predicted_labels <- extract(classified_image, test_points)
  confusion_matrix <- table(True = true_labels, Predicted = predicted_labels)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  return(list(confusion_matrix = confusion_matrix, accuracy = accuracy))
}

# Calculate accuracy for each year
accuracy_list <- lapply(names(classified_images), function(year) {
  classified_image <- classified_images[[year]]
  ndvi_composite <- yearly_composites[[year]]
  accuracy_data <- calculate_accuracy(classified_image, ndvi_composite, num_samples, unchanged_pixels)
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
