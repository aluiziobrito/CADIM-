# ============================================================
# 1 - LOAD NECESSARY LIBRARIES
# ============================================================
# List of required packages
neededPackages <- c(
  "raster", "stats", "sf", "ggplot2", "sp", "dplyr", "tidyr", "ROCR",
  "reshape2", "randomForest", "caret", "caTools", "geobr", "prettymapr", 
  "tidyselect", "Boruta", "corrplot", "terra", "cluster", "parallel", "shiny"
) 
pkgTest <- function(package) { 
  if (!require(package, character.only = TRUE)) { 
    install.packages(package, dependencies = TRUE) 
    library(package, character.only = TRUE) 
  }
}
sapply(neededPackages, pkgTest)


# ============================================================
# 2 - DEFINE AND CHECK ATTRIBUTES
# ============================================================

# Define file paths
cloud_mask_path <- "D:\\Dissertation\\Test_Area\\OFFICIAL_CLOUD_SHADOW_MASK.tif"
rasters_path <- "D:\\Dissertation\\Test_Area\\Attributes_RF1_test"
reference_raster <- "D:\\Dissertation\\Test_Area\\Attributes_RF1_test\\AWEinsh.tif"  # for resampling 


# List all .tif files in the specified directory
input_files <- list.files(
  path = rasters_path,       # Directory path
  pattern = "\\.tif$",       # Search only for files with .tif extension
  full.names = TRUE          # Return full file paths
)

# Check the number of files found
num_files <- length(input_files)  # Count the number of listed files

if (num_files == 0) {
  
  stop("No .tif files found in the specified directory.")
} else {
  cat("Number of .tif files found:", num_files, "\n")
}

# ============================================================
# 3 - ADJUST ATTRIBUTES
# ============================================================

adjust_raster <- function(raster_path, reference) {
  cat("Processing..:", raster_path, "\n")
  
  # Load the raster and the mask
  r <- rast(raster_path)
  
  # Check and adjust the CRS of the raster
  cat("  Checking raster CRS\n")
  if (!compareCRS(r, reference)) {
    cat("  Different projections. Reprojecting raster...\n")
    r <- project(r, crs(reference))
  }
  
  # Resample the raster to match the resolution and extent of the reference raster
  cat("  Resampling raster.\n")
  r <- resample(r, reference, method = "bilinear") # For continuous data
  
  # Adjust the raster's extent to match the reference raster's extent
  cat("  Checking raster extent\n")
  if (ext(r) != ext(reference)) {
    cat("  Different extents. Cropping raster...\n")
    r <- crop(r, ext(reference))
  }
  
  return(r)
}

# Load the reference raster 
raster_reference <- rast(raster_referencia)
plot(raster_reference)

# Load the cloud mask
cloud_mask <- rast(caminho_mascara_nuvens)
plot(cloud_mask)

# Adjust the cloud mask to match the reference raster
adjust_cloud_mask <- function(cloud_mask, raster_reference) {
  cat("Adjusting cloud mask...\n")
  
  # Reproject the cloud mask to match the reference raster's CRS if necessary
  if (!compareCRS(cloud_mask, raster_reference)) {
    cat("  Reprojecting cloud mask...\n")
    cloud_mask <- project(cloud_mask, crs(raster_reference))
  }
  
  # Resample the cloud mask to match the reference raster's resolution
  cat("  Resampling cloud mask...\n")
  cloud_mask <- resample(cloud_mask, raster_reference, method = "near")
  
  # Crop the cloud mask to match the reference raster's extent
  cat("  Cropping cloud mask...\n")
  cloud_mask <- crop(cloud_mask, ext(raster_reference))
  
  return(cloud_mask)
}

# Adjust the cloud mask
adjusted_cloud_mask <- adjust_cloud_mask(cloud_mask, raster_reference)
plot(adjusted_cloud_mask)

# Call the function to adjust the raster
adjust_raster <- function(raster_path, reference, mask) {
  cat("Processing:", basename(raster_path), "\n")
  r <- rast(raster_path)
  
  # Check and adjust the CRS
  if (!compareCRS(r, reference)) {
    cat("  CRS different. Reprojecting...\n")
    r <- project(r, crs(reference))
  }
  
  # Resample to match the reference resolution
  cat("  Resampling...\n")
  r <- resample(r, reference, method = "bilinear")
  
  # Crop to match the reference extent
  cat("  Cropping...\n")
  r <- crop(r, ext(reference))
  
  # Check the mask extent
  if (ext(mask) != ext(r)) {
    cat("  Mask extent does not match raster. Adjusting mask...\n")
    mask <- crop(mask, ext(r))
  }
  
  # Apply the cloud mask
  cat("  Applying cloud mask...\n")
  r <- mask(r, mask, maskvalue = 1)
  
  # Check if the raster is valid
  if (!inherits(r, "SpatRaster")) {
    cat("  Error: Not a valid SpatRaster object.\n")
    return(NULL)
  }
  
  return(r)
}

# Adjust all rasters to the same resolution, extent, and projection
adjusted_rasters <- list()
for (i in seq_along(arquivos_entrada)) {
  cat("Processing raster", i, "of", length(arquivos_entrada), "\n")
  try({
    adjusted_raster <- adjust_raster(arquivos_entrada[i], raster_reference, adjusted_cloud_mask)
    if (!is.null(adjusted_raster)) {
      adjusted_rasters[[length(adjusted_rasters) + 1]] <- adjusted_raster
    } else {
      cat("  Raster not adjusted correctly.\n")
    }
  }, silent = FALSE)
}

# Check if any raster was adjusted
if (length(adjusted_rasters) == 0) {
  stop("No raster was adjusted correctly.")
}

# Check the type of objects in the list
print(sapply(adjusted_rasters, class))


# Stack the adjusted rasters
stack <- rast(adjusted_rasters)

Attributes <- stack # here, I renamed the stack to "Attributes"
plot(Attributes) 
print(Attributes)
summary(Attributes)


# ============================================================
# 5 - RANDOM FOREST MODEL
# ============================================================

amostras_atributos$Classe <- as.factor(amostras_atributos$Classe)

# Running Boruta for variable selection
set.seed(1)
boruta_result <- Boruta(Classe ~ ., data = amostras_atributos, doTrace = 2)

# Get the truly important variables
importantes <- getSelectedAttributes(boruta_result, withTentative = FALSE)
print(importantes)

# Subset of data with only the variables selected by Boruta
variaveis_importantes <- amostras_atributos[, c(importantes, "Classe")]

boruta_importancia <- attStats(boruta_result)

# Visualizing the ranking
print(boruta_importancia)

# Sorting the variables based on importance (in descending order)
boruta_importancia <- boruta_importancia[order(boruta_importancia$meanImp, decreasing = TRUE), ]

# Visualizing the sorted variables
print(boruta_importancia)

# Creating the attribute importance plot

ggplot(boruta_importancia, aes(x = reorder(rownames(boruta_importancia), meanImp), y = meanImp)) +
  geom_bar(stat = "identity", fill = "#69b3a2", color = "black") +  
  coord_flip() + 
  labs(title = " ",
       x = "Attributes",
       y = "Mean Importance") +
  theme_minimal(base_size = 15) +  
  theme(
    panel.background = element_rect(fill = "lightgrey"),  
    plot.background = element_rect(fill = "lightgrey"),   
    panel.grid.major = element_line(color = "white"),      
    panel.grid.minor = element_line(color = "white"),      
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"), 
    axis.title = element_text(size = 14)                    
  )


# Correlation matrix between the predictor variables
correlacao <- cor(amostras_atributos %>% select(-Classe))
corrplot::corrplot(correlacao, method = "circle")


# MODEL ADJUSTMENT WITH INTERFACE

# User interface
ui <- fluidPage(
  titlePanel("Random Forest Adjustment"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("ntree", 
                  "Number of Trees (ntree):", 
                  min = 100, 
                  max = 2000, 
                  value = 800, 
                  step = 50),
      
      sliderInput("mtry", 
                  "Number of Variables per Node (mtry):", 
                  min = 1, 
                  max = length(variaveis_importantes), 
                  value = floor(sqrt(length(variaveis_importantes))), 
                  step = 1),
      
      actionButton("train", "Train Model"),
      actionButton("saveModel", "Save Model")  # New button to save the model
    ),
    
    mainPanel(
      plotOutput("oobPlot"),
      verbatimTextOutput("modelSummary")
    )
  )
)


server <- function(input, output, session) {
  
  # Reactive variable to store the model
  modelo_rf <- reactiveVal(NULL)
  
  # Train the model when the button is clicked
  observeEvent(input$train, {
    set.seed(2)
    
    # Subset of data with the variables selected by Boruta
    amostras_atributos_boruta <- amostras_atributos
    
    # Training the Random Forest model
    modelo <- randomForest(Classe ~ ., 
                           data = amostras_atributos_boruta, 
                           ntree = input$ntree, 
                           mtry = input$mtry, 
                           importance = TRUE)
    assign("modelo_rf", modelo, envir = .GlobalEnv)
    
    modelo_rf(modelo)
  })
  
  # Display the model metrics
  output$modelSummary <- renderPrint({
    req(modelo_rf())  # Ensures the model has been trained
    print(modelo_rf())
  })
  
  # Plot the OOB error
  output$oobPlot <- renderPlot({
    req(modelo_rf())  # Ensures the model has been trained
    
    oob_error <- as.data.frame(modelo_rf()$err.rate) %>% 
      mutate(ntree = as.numeric(row.names(.)))
    
    ggplot(oob_error, aes(x = ntree, y = OOB)) +
      geom_line(color = "darkblue", size = 1) +    
      geom_point(color = "red", size = 1) +      
      theme_bw(base_size = 14) +                    
      labs(
        title = "OOB Error vs. Number of Trees",  
        x = "Number of Trees",                          
        y = "OOB Error"                                    
      ) +
      theme(
        axis.title.x = element_text(size = 14, face = "bold"),    
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  
        panel.grid.major = element_line(size = 0.5, color = "gray80"),     
        panel.grid.minor = element_blank(),                              
        panel.border = element_rect(color = "black", fill = NA)           
      )
  })
  
  
  observeEvent(input$saveModel, {
    req(modelo_rf())  
    
    # Save the model 
    saveRDS(modelo_rf(), file = "D:\\Dissertation\\Test_Area\\Saved\\RF2\\Models\\Final_Model")
    
    # Show success message
    showModal(modalDialog(
      title = "Success",
      "Model saved successfully!",
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


# ============================================================
# 6 - PREDICT - Classification Process
# ============================================================

# Classify based on the rf model
rf.class <- raster::predict(Atributos, modelo_rf, progress = "text", type = "response")
plot(rf.class)

# Write the raster object to a file
writeRaster(rf.class, "D:\\Dissertation\\Test_Area\\Saved\\RF2\\Classifications\\class_8.tif", overwrite = TRUE)

# ============================================================
# 7 - Shannon Entropy
# ============================================================

# Define the paths where the files will be saved
Probabilidade <- "D:\\Dissertation\\Test_Area\\Saved\\RF2\\prob_8.tif"
Entropia_Caminho = "D:\\Dissertation\\Test_Area\\Saved\\RF2\\Uncertainty\\entropy_8.tif"
Amostras_Entropia = "D:\\Dissertation\\Test_Area\\Saved\\RF2\\Uncertainty\\entropy_samples_"

# Extract probability information from the classification
prob_RF <- raster::predict(Atributos, modelo_rf, type = "prob") 
print(prob_RF)
plot(prob_RF)
writeRaster(prob_RF, filename = Probabilidade, overwrite = TRUE)

# Calculate the entropy
entropy <- - ((prob_RF$`X1` * log2(prob_RF$`X1`)) + (prob_RF$`X0` * log2(prob_RF$`X0`)))
entropy[is.na(entropy)] = 0.000 # Prevents log2(0)
plot(entropy)
print(entropy)

# Apply cloud mask
entropy <- mask(entropy, mascara_nuvens_ajustada, maskvalue = 1)
plot(entropy)

# Save the entropy raster
writeRaster(entropy, filename = Entropia_Caminho, overwrite = TRUE)
