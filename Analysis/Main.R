
packages <- c(
  "googledrive", "tidyverse",  "terra", "npphen", "raster", "RColorBrewer", 
  "rts", "lubridate", "dplyr", "parallel", "sf", "readxl", "landscapemetrics", 
  "car", "stargazer", "ggeffects", "patchwork", "betareg"
)

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

funs <- list.files("C:/Users/Bianka/Documents/MSc-Thesis/Analysis/R/", pattern = "\\.R$", full.names = TRUE)
lapply(funs, source)

dry_forest <- "../../../04_data/forest/df/"
wet_forest <- "../../../04_data/forest/wf/"

dry_forest_seed <- "../../../04_data/seed/df/"
wet_forest_seed <- "../../../04_data/seed/wf/"

# authenticate and download NDMI Landsat data from Google drive
drive_auth()
retrieve_sd("landsat_92-22_df", file.path(dry_forest, "05_satellite"))
retrieve_sd("landsat_92-22_wf", file.path(wet_forest, "05_satellite"))

# Run avocado algorithm
# due to high computational power last part can only be run on a remote server = commented out
avocado(dry_forest)
avocado(wet_forest)

# Compute forest mosaic
forest_mosaic(dry_forest)
forest_mosaic(wet_forest)

# Calculate forest variables
compute_ff(dry_forest)
compute_ff(wet_forest)

# Calculate seed variables
compute_sf(dry_forest_seed)
compute_sf(wet_forest_seed)

# run the statistics script
source("C:/Users/Bianka/Documents/MSc-Thesis/Analysis/statistics.R")


