# 🌊☁️ **Cloud-Aware DEM Inundation Mapping (CADIM)** 🌊☁️

 A geospatial framework for flood mapping under cloud cover using machine learning and digital elevation models (DEMs).

**Overview** 

CADIM is an innovative approach designed to overcome the challenges of cloud-covered optical imagery in flood mapping. By integrating spectral indices, hydrology, and machine learning techniques, CADIM enables accurate detection of inundated areas and enhances emergency response in flood-prone regions.

**Key Features**

- *Cloud and Shadow Detection*: Advanced spectral indices and decision tree models (Rpart) for improved cloud and shadow masking.
- *Flood Mapping*: Leveraging DEM attributes with Random Forest to detect flooded areas.
- *Prediction Under Clouds*: Accurate flood mapping even in areas with cloud cover.
- *Scalability*: Modular and reproducible framework suitable for diverse regions, datasets and even applications.

**Applications**
- Flood risk assessment in climate-sensitive regions.
- Emergency response planning and resilience building.
- Integration with real-time hydrological monitoring systems.

**🔺🤏Why the name CADIM?🔺🤏**

The name CADIM stands for Cloud-Aware DEM Inundation Mapping. But it’s more than just an acronym! In Mineirês-Portuguese, "cadim" (pronounced kah-jeem) is a colloquial way of saying "a little bit". It reflects the idea of doing "a little bit more" to overcome the challenges of flood mapping under cloudy skies, Even if it's just a little step towards Everest. While traditional methods struggle when clouds blocking the view, CADIM takes it a step further by integrating Digital Elevation Models and machine learning to ensure nothing is missed. 


The set of codes includes:

I - The file **"Det_n_s_final.R"**, which contains the code for detecting and creating the cloud and shadow mask using the Rpart decision tree classifier. Also, it includes the 'Clouds_Shadows_Mask.md' file, which provides documentation and supplementary details for the code.

II - The file **kmeans.R** uses the unspervised ML algorithm to generate the initial samples.

III - The file **morpho_fil.R** is used to filter the morphometrical attributes using the gaussian filter.

IV - The file **"RF1.R"**, which includes all stages (parameterization, sampling, classification, and evaluation) of mapping flooded areas using the Random Forest classifier, and The file **"RF2.R"**, which includes all stages (parameterization, sampling, classification that are the same as in RF1, and also a under-cloud prediction) of mapping flood-prone areas using the Random Forest classifier.

V - The **boruta.R** file includes the attribute selection tool with the Boruta Algoritm.

Any reference to the code and the work can be made by citing:

 A. B. Maia, C. D. Rennó, E. M. L. M. Novo, I. R. C. Mira. "A DEM-Based Model for Fast Flood Detection Under Simulated Cloud Cover: A Controlled Study." **Some Journal infos**. 2025

And any furter informations can be acquired in:

**MAIA, A. B.** Detection of Flooded Areas Under Cloud Cover in Optical Systems: Integration with Digital Elevation Model. Thesis (Master’s in Remote Sensing) - National Institute for Space Research (INPE), São José dos Campos, 2025.

Feel free to contribute! If you have suggestions, don’t hesitate to open an issue or a pull request.

For more information and discussions, contact:
Aluizio Brito Maia aluizio.maia@inpe.br  
Camilo Daleles Rennó camilo.renno@inpe.br  

## Installation 
To install all of the required packages, run in your R environment:

```R
# List of required packages
neededPackages = c("viridis", "terra", "raster", "stats", "sf", "ggplot2", "sp", "dplyr", "tidyr", "ROCR",
                   "reshape2", "randomForest", "caret", "caTools", "geobr", "prettymapr", 
                   "tidyselect", "rpart", "rpart.plot", "partykit")

# Function to check if the package is installed. If not, it will be installed and loaded.
pkgTest = function(x) {
  if (!x %in% rownames(installed.packages())) { 
    install.packages(x, dependencies = TRUE) 
  }
  library(x, character.only = TRUE)
}
for (package in neededPackages) {
  pkgTest(package)
}




