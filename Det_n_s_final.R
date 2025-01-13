# ==== LOAD SPECTRAL ATTRIBUTES ====

# Define the directory where the files are located
dir_path <- "Insert\\here\\your\\path.tif"

# List all .tif files in the folder
files_list <- list.files(path = dir_path, pattern = "\\.tif$", full.names = TRUE)

# Load the raster files
rasters_list <- lapply(files_list, raster)

# Name each element of the list with the filename (without .tif extension)
names(rasters_list) <- tools::file_path_sans_ext(basename(files_list))

# Print the list of rasters to see if its okay
print(rasters_list)

# Create a stack with the rasters from the list
# For details on attributes, refer to the Mark Down Clouds_Shadows_Mask.md
img <- stack(rasters_list[["Wv"]],        # Water vapor (Wv)
             rasters_list[["WI"]],        # Water index (WI)
             rasters_list[["HOT"]],       # Hotness index (HOT)
             rasters_list[["SWIR_SCD"]],  # Short-Wave Infrared Spectral Cloud Detection (SWIR_SCD)
             rasters_list[["NIR-SCD"]],   # Near-Infrared Spectral Cloud Detection (NIR-SCD)
             rasters_list[["Cloud Index"]], # Cloud index
             rasters_list[["Cirrus"]])    # Cirrus clouds

# Plot the stack of rasters
plot(img)

# ==== SAMPLING STAGE ====

# Load shapefiles and standardize the CRS for all
samples <- list.files("Insert\\here\\your\\path", 
                      pattern = ".shp", full.names = TRUE) %>%
  lapply(st_read) %>%
  lapply(function(x) st_transform(x, st_crs(4326))) # WGS84 CRS is used as an example, adjust if necessary

# Combine shapefiles into a single 'sf' object
samples_combined <- do.call(rbind, samples)

# Extract raster values for the sample points
sample_attributes <- raster::extract(img, samples_combined)

# Convert to dataframe and add the 'Class' column
attributes_df <- as.data.frame(sample_attributes)
sample_data <- st_drop_geometry(samples_combined)

# Update the 'id' column to the desired class
sample_data$id <- ifelse(sample_data$id == 3, 1, sample_data$id)

# Merge class data with extracted attributes
samples_attributes <- data.frame(Class = sample_data$id, attributes_df)

# Split the samples into training and testing sets (In this case, 70% for training, adjust if necessary)
index <- createDataPartition(samples_attributes$Class, p = 0.7, list = FALSE)
training_samples <- samples_attributes[index, ]
testing_samples <- samples_attributes[-index, ]

# Print the combined samples attributes
print(samples_attributes)

# ==== R-PART MODEL ====

train_and_evaluate <- function(data) {
  set.seed(100)  
  num_folds <- 10 # Adjust as necessary, values between 5 and 10 are recommended
  folds <- createFolds(data$Class, k = num_folds, list = FALSE)  # Create each fold
  
  metrics_df <- data.frame(Accuracy = numeric(num_folds),
                           Sensitivity = numeric(num_folds),
                           Specificity = numeric(num_folds),
                           Precision = numeric(num_folds),
                           F1_Score = numeric(num_folds))
  
  for (i in 1:num_folds) {
    train_index <- which(folds != i)
    test_index <- which(folds == i)
    
    # Training and testing data
    train_data <- data[train_index, ]
    test_data <- data[test_index, ]
    
    # Train the model # Adjust hyperparameters as needed, refer to the Markdown Clouds_Shadows_Mask.md.
    decision_tree <- rpart(
      Class ~ ., 
      data = train_data,
      method = "class",
      control = rpart.control(
        minsplit = 10,
        minbucket = round(10 / 3),
        cp = 0.001,
        maxcompete = 4, 
        maxsurrogate = 1,
        usesurrogate = 2, 
        xval = 0,  
        surrogatestyle = 0, 
        maxdepth = 20
      )
    )
    
    # Evaluate the model
    predicted_class <- predict(decision_tree, test_data, type = "class")
    
    # Convert to the same reference
    test_data$Class <- factor(test_data$Class, levels = levels(predicted_class))
    ConfusionMatrix <- confusionMatrix(data = predicted_class, reference = test_data$Class)
    
    # Store metrics
    metrics_df[i, "Accuracy"] <- ConfusionMatrix$overall['Accuracy']
    metrics_df[i, "Sensitivity"] <- ConfusionMatrix$byClass['Sensitivity']
    metrics_df[i, "Specificity"] <- ConfusionMatrix$byClass['Specificity']
    metrics_df[i, "Precision"] <- ConfusionMatrix$byClass['Precision']
    metrics_df[i, "F1_Score"] <- ConfusionMatrix$byClass['F1']
    
    # Print metrics for each fold
    cat("Fold:", i, "\n")
    cat("Accuracy:", metrics_df[i, "Accuracy"], "\n")
    cat("Sensitivity:", metrics_df[i, "Sensitivity"], "\n")
    cat("Specificity:", metrics_df[i, "Specificity"], "\n")
    cat("Precision:", metrics_df[i, "Precision"], "\n")
    cat("F1 Score:", metrics_df[i, "F1_Score"], "\n")
    cat("----------------------\n")
    
    # Plot one by one
    readline(prompt = "Press Enter in the console to continue to the next fold :) ...")
  }
  
  # Generate long-format metric names
  metrics_df_long <- melt(as.data.frame(metrics_df))
  colnames(metrics_df_long) <- c("Metric", "Value")
  
  # Replace metric names for English or the desired language
  metrics_df_long$Metric <- factor(metrics_df_long$Metric, 
                                   levels = c("Accuracy", "Sensitivity", "Specificity", "Precision", "F1_Score"),
                                   labels = c("Accuracy", "Sensitivity", "Specificity", "Precision", "F1"))
  
  # Generate boxplots for each metric 
  for (metric in unique(metrics_df_long$Metric)) {
    individual_plot <- ggplot(subset(metrics_df_long, Metric == metric), aes(x = Metric, y = Value)) + 
      geom_boxplot(fill = "lightblue", color = "darkblue", outlier.color = "red", outlier.shape = 16) + 
      stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "darkred", fill = "darkred") +
      labs(title = paste("Performance -", metric),
           x = "Metric",
           y = "Values") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 12, face = "bold", color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold")
      )
    
    # Display plot
    print(individual_plot)
    
    # Display plots
    readline(prompt = "Press Enter in the console to see the next plot :) ...")
  }
  
  # Generate Confidence Intervals
  confidence_intervals <- data.frame(
    Metrics = colnames(metrics_df),
    Mean = colMeans(metrics_df),
    CI_Lower = apply(metrics_df, 2, function(x) mean(x) - qt(0.975, df = num_folds - 1) * sd(x) / sqrt(num_folds)),
    CI_Upper = apply(metrics_df, 2, function(x) mean(x) + qt(0.975, df = num_folds - 1) * sd(x) / sqrt(num_folds))
  )
  
  # Confidence Interval plot
  ggplot(confidence_intervals, aes(x = Metrics, y = Mean)) + 
    geom_point() +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
    labs(title = "Confidence Intervals for Performance Metrics",
         x = "Metrics",
         y = "Values") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Angle text on x-axis
}

# Call the function
train_and_evaluate(samples_attributes)
summary(decision_tree)

#   4 -    ==== PREDICTION ==== 

filtrpart <- rpart(
  Class ~ ., 
  data = treino_amostras,
  method = "class",
  control = rpart.control(
    minsplit = 10,
    minbucket = round(10 / 3),
    cp = 0.001,
    maxcompete = 4, 
    maxsurrogate = 1,
    usesurrogate = 2, 
    xval = 0,  
    surrogatestyle = 0, 
    maxdepth = 20
  )
)

# Check if the model was trained correctly and if the image is a raster
if (!is.null(filtrpart) && inherits(img, "Raster")) {
  
  # Predict
  rpart.pred <- raster::predict(img, filtrpart, progress = "text", type = "class", na.rm = TRUE)
  
  # Plot the result
  plot(rpart.pred, main = "Prediction of the rpart Model", col = viridis(2))  #
  
  # Name the output file...
  output_file <- "Class_rpart_Todos_atbA2if.tif"
  
  # If the file exists, ask if you want to overwrite
  if (file.exists(output_file)) {
    message("The file already exists. Do you want to overwrite it? (y/n)")
    resposta <- readline()
    if (tolower(resposta) != "y") {
      message("The file was not saved.")
    } else {
      writeRaster(rpart.pred, filename = output_file, overwrite = TRUE)
      message("File saved with the name: ", output_file)
    }
  } else {
    writeRaster(rpart.pred, filename = output_file, overwrite = TRUE)
    message("File saved with the name: ", output_file)
  }
  
} else {
  message("Error: The model or the image are not in the expected format.")
}

rpart.pred_values <- raster::values(rpart.pred)


#   5 -  ==== EVALUATION/TESTING ==== 

# Function to calculate the MCC
calculate_mcc <- function(vp, vn, fp, fn) {
  numerator <- (vp * vn) - (fp * fn)
  denominator <- sqrt((vp + fp) * (vp + fn) * (vn + fp) * (vn + fn))
  
  if (denominator == 0) {
    return(0)
  }
  
  mcc <- numerator / denominator
  return(mcc)
}
# Evaluating the model with random samples

evaluate_model <- function(sample_path, img, rpart_pred) {
  # Load random samples
  random_samples <- st_read(sample_path)  
  
  # Check if the "Class" column exists in the layer
  if (!"Class" %in% colnames(random_samples)) {
    stop("The 'Class' column does not exist.")
  }
  
  # Transform CRS of the samples
  random_samples <- st_transform(random_samples, crs(rpart_pred))
  
  # Extract the raster values predicted by rpart
  predicted_values <- raster::extract(rpart_pred, random_samples)
  
  # Get the sample values
  true_values <- random_samples$Class
  
  # Calculate performance metrics
  # Pay attention to the numbers used for the classes and change if necessary, in this case, 1 represents clouds/shadows and 2 represents other surfaces
  vp <- sum(predicted_values == 1 & true_values == 1, na.rm = TRUE)
  vn <- sum(predicted_values == 2 & true_values == 2, na.rm = TRUE)
  fp <- sum(predicted_values == 1 & true_values == 2, na.rm = TRUE)
  fn <- sum(predicted_values == 2 & true_values == 1, na.rm = TRUE)
  
  # Global Accuracy
  ag <- (vp + vn) / (vp + vn + fp + fn)
  
  # Sensitivity
  sensitivity <- ifelse((vp + fn) == 0, 0, vp / (vp + fn))
  
  # Precision
  precision <- ifelse((vp + fp) == 0, 0, vp / (vp + fp))
  
  # F1-Score
  f1 <- ifelse((precision + sensitivity) == 0, 0, 2 * (precision * sensitivity) / (precision + sensitivity))
  
  # Specificity
  specificity <- ifelse((vn + fp) == 0, 0, vn / (vn + fp))
  
  # Matthew Correlation Coefficient
  mcc <- calculate_mcc(vp, vn, fp, fn)
  
  results <- list(
    Accuracy = ag,
    Sensitivity = sensitivity,
    Precision = precision,
    F1 = f1,
    Specificity = specificity,
    False_Positives = fp,
    False_Negatives = fn,
    MCC = mcc
  )
  
  return(results)
}
results <- evaluate_model("D:\\Dissertation\\Clouds and Shadows\\Decision Tree\\Test_Samples", img, rpart.pred)
print(results)

#   6 -  ==== Probability Functions and Classification based on Probability ====

# Extract class probabilities
rpart.pred_prob <- raster::predict(img, filtrpart, progress = "text", type = "prob", na.rm = TRUE)

# Visualize the probability of the first class to check if everything is correct
plot(rpart.pred_prob[[1]], main = "Probability of Class 1")

# Save the probability raster
probs <- "D:\\Dissertation\\Clouds and Shadows\\Decision Tree\\class_probability.tif"
writeRaster(rpart.pred_prob, filename = probs, overwrite = TRUE)

# Chosen probability threshold
prob_threshold = 0.8

# Classification based on the probability of the second class (class 2)
rpart.pred_class_prob <- calc(rpart.pred_prob, fun = function(x) {
  class_prob <- x[1]  
  
  # Apply the probability condition: if greater than the threshold, classify as "cloud", otherwise "surface"
  ifelse(class_prob > prob_threshold, 1, 2)
})

plot(rpart.pred_class_prob, main = paste("Classification with Probability Threshold of", prob_threshold))
writeRaster(rpart.pred_class_prob, filename = "D:\\Dissertation\\Clouds and Shadows\\Decision Tree\\binary_classification.tif", overwrite = TRUE)


