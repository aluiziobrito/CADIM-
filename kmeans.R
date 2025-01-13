# ========= K-means Application for Sampling ==========
# This script processes rasters, applies K-means for sampling,
# adjusts projections/resolutions, and generates random samples by class.

# =========== 0. Start =============
library(terra)
library(cluster)
library(parallel)
library(sf)

# ========== 1. Auxiliary Functions ==========
# Function to adjust a raster to the reference, apply cloud mask and normalize
adjust_and_normalize_raster <- function(raster_path, ref_raster, cloud_mask) {
  cat("\nProcessing:", basename(raster_path), "\n")
  
  r <- rast(raster_path)
  
  # Adjust CRS
  if (!compareCRS(r, ref_raster)) {
    cat("  CRS different. Reprojecting...\n")
    r <- project(r, crs(ref_raster))
  }
  
  # Resample resolution
  cat("  Resampling...\n")
  r <- resample(r, ref_raster, method = "bilinear")
  
  # Crop to match reference extent
  cat("  Cropping...\n")
  r <- crop(r, ext(ref_raster))
  
  # Adjust cloud mask
  if (ext(cloud_mask) != ext(r)) {
    cat("  Adjusting cloud mask extent...\n")
    cloud_mask <- crop(cloud_mask, ext(r))
  }
  
  # Apply cloud mask
  cat("  Applying cloud mask...\n")
  r <- mask(r, cloud_mask, maskvalue = 1)
  
  # Interquartile normalization
  cat("  Normalizing values...\n")
  values <- values(r)
  normalized_values <- normalize_interquartile(values)
  values(r) <- normalized_values
  
  return(r)
}

# Function for interquartile normalization
normalize_interquartile <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  
  if (Q3 == Q1) {
    warning("Quartiles are equal, normalization will not be done.")
    return(rep(0, length(x)))
  }
  
  x_truncated <- pmin(pmax(x, Q1), Q3)
  return((x_truncated - Q1) / (Q3 - Q1))
}

# ========== 2. Load Data ==========
# Define paths
path_to_rasters <- "D:\\Dissertation\\Test_Area\\Attributes_RF2_test"
cloud_mask_path <- "D:\\Dissertation\\Test_Area\\CLOUD_SHADOW_MASK_OFFICIAL.tif"
ref_raster_path <- "D:\\Dissertation\\Test_Area\\Attributes_RF1_test\\AWEinsh.tif"

# Load data
input_files <- list.files(path = path_to_rasters, pattern = ".tif$", full.names = TRUE)
ref_raster <- rast(ref_raster_path)
cloud_mask <- rast(cloud_mask_path)

# Adjust the cloud mask
cloud_mask <- adjust_and_normalize_raster(cloud_mask_path, ref_raster, ref_raster)

# Check if files exist
if (length(input_files) == 0) stop("No .tif files found.")

# ========== 3. Adjust and Normalize Rasters ==========
cat("\nAdjusting and normalizing rasters...\n")
normalized_rasters <- lapply(input_files, function(x) {
  tryCatch({
    adjust_and_normalize_raster(x, ref_raster, cloud_mask)
  }, error = function(e) {
    cat("Error processing", x, ":", e$message, "\n")
    return(NULL)
  })
})

# Remove invalid rasters
normalized_rasters <- Filter(Negate(is.null), normalized_rasters)

if (length(normalized_rasters) == 0) stop("No raster adjusted and normalized correctly.")

# Stack the normalized rasters
stack <- rast(normalized_rasters)
cat("Rasters stacked and normalized.\n")

# Visualize to check the result
plot(stack)

# ========== 5. Apply K-means ==========
cat("\nApplying K-means...\n")
kmeans_result <- kmeans(na.omit(values(stack)), centers = 10, nstart = 25, iter.max = 500)

# Create an empty raster based on the stack
cluster_raster <- stack[[1]] 
values(cluster_raster) <- NA # Set all values as NA
stack_values <- values(stack)

# Apply K-means
kmeans_result <- kmeans(na.omit(stack_values), centers = 10, nstart = 25, iter.max = 500)

# Identify valid cells
if (is.null(dim(stack_values))) {
  # If it's a vector (single layer raster)
  valid_cells <- !is.na(stack_values)
} else {
  # If it's a matrix (multi-layer raster)
  valid_cells <- !is.na(rowSums(stack_values, na.rm = TRUE))
}

# Fill the empty raster with clusters
values(cluster_raster)[valid_cells] <- kmeans_result$cluster
plot(cluster_raster)
# Save the classified raster
writeRaster(cluster_raster, "D:\\Dissertation\\Test_Area\\Kmeans_Samples\\Raster_kmeans.tif", overwrite = TRUE)
