# Loading the raster file
declividade <- raster("Insert here your tif")

# Gaussian kernel function definition
gaussian_kernel <- function(size, sigma) {
  center <- floor(size / 2)
  kernel <- matrix(0, nrow = size, ncol = size)
  for (i in 1:size) {
    for (j in 1:size) {
      x <- i - center - 1
      y <- j - center - 1
      kernel[i, j] <- exp(-(x^2 + y^2) / (2 * sigma^2))
    }
  }
  kernel <- kernel / sum(kernel)  # Normalize the kernel
  return(kernel)
}

# Defining kernel parameters
kernel_size <- 7  # Kernel size (7x7)
sigma <- 1        # Standard deviation

# Create the Gaussian kernel
gaussian_k <- gaussian_kernel(kernel_size, sigma)

# Apply the Gaussian filter using the focal function
declividade_smooth <- focal(declividade, w = gaussian_k)

# Visualizing the smoothed slope map
levelplot(declividade_smooth, main = "Smooth Slope")

# Save the smoothed raster to a file
writeRaster(declividade_smooth, filename = "where to save", format = "GTiff", overwrite = TRUE)

# Plot the original raster for comparison
plot(declividade)