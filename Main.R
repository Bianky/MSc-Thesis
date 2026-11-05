
packages <- c(
  "googledrive", "tidyverse",  "terra", "npphen", "raster", "RColorBrewer", 
  "rts", "lubridate", "dplyr", "parallel", "sf", "readxl", "landscapemetrics", 
  "car", "stargazer", "ggeffects", "patchwork", "betareg", "performance", "lmerTest",
  "MuMIn", "spdep", "tmap", "Hmisc", "corrplot"
)


for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

funs <- list.files("MSc-Thesis/R/", pattern = "\\.R$", full.names = TRUE)
lapply(funs, source)

dry_forest <- "MSc-Thesis/data/forest/df"
wet_forest <- "MSc-Thesis/data/forest/wf"

dry_forest_seed <- "MSc-Thesis/data/seed/df"
wet_forest_seed <- "MSc-Thesis/data/seed/wf"

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

# Calculate seed variables per season
compute_sf_season(dry_forest_seed)
compute_sf_season(wet_forest_seed)

# run the statistics
source("MSc-Thesis/statistics/table formation.R")
source("MSc-Thesis/statistics/correlation.R")
source("MSc-Thesis/statistics/ttests.R")
source("MSc-Thesis/statistics/models.R")
source("MSc-Thesis/statistics/Moran's I.R")
source("MSc-Thesis/statistics/figures.R")




