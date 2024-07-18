library(terra)

# Path to NDVI images
ndvi_path <- "C:/Users/Jado/Documents/EAGLE/Semester2/Cloud_computing/predicted_rasters"

# Define output path to save classified images
output_path <- "D:/Second_semester/BinClass/Monthly_BinClass"


# Function to read and classify a single NDVI image
BinClassMonthly <- function(file, output_path, threshold = 0.45) {
  # Read the NDVI image
  ndvi_image <- rast(file)
  
  # Reclassify based on the threshold: 1 for NonForest, 2 for Forest
  rcl <- matrix(c(-Inf, threshold, 1, threshold, Inf, 2), ncol = 3, byrow = TRUE)
  classified_image <- classify(ndvi_image, rcl = rcl)
  
  # Assign labels to the classified values
  levels(classified_image) <- data.frame(ID = c(1, 2), Class = c("NonForest", "Forest"))
  
  # Create output file path
  output_file <- gsub("predicted_ndvi_", "classified_", file)
  output_file <- file.path(output_path, basename(output_file))
  
  # Save the classified image
  writeRaster(classified_image, filename = output_file, filetype = "GTiff", overwrite = TRUE)
  
  # Free memory
  rm(ndvi_image)
  gc()
}

# Get list of NDVI image files
file_list <- list.files(ndvi_path, pattern = "predicted_ndvi_.*\\.tif$", full.names = TRUE)

# Process and classify each image individually
for (file in file_list) {
  BinClassMonthly(file, output_path)
}

