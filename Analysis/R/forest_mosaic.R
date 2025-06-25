forest_mosaic <- function(data_folder) {
  # function to create a forest mosaic with non-forest areas and forest areas with the most recent regrowth years
  # data_folder is a folder where data is stored
  
  ## desired output classes
  # 92-22 = last regrowth
  # 0 = non forest area or disturbance as last
  # 1 = forest that has never been disturbed
  
  # disturbance regrowth map (result of AVOCADO)
  distreg <- rast(file.path(data_folder, "08_landsat_1992_2022_DistReg.tif"))
  
  # area of interest
  aoi <- project(vect(file.path(data_folder, "04_extent.shp")), crs(distreg))
  
  # ESA land cover map (2021)
  landcover <- rast(file.path(data_folder, "09_ESA_2021_landcover.tif"))
  
  lc <- crop(landcover, aoi)
  lc <- mask(lc, aoi)
  
  # resampling to make sure both rasters align
  lc_resampled <- resample(lc, distreg, method = "near") 
  
  # add a new layer to the AVOCADO forest map output
  distreg$lyr.17 <- lc_resampled
  
  ## ESA 2021 landcover classes
  # https://developers.google.com/earth-engine/datasets/catalog/ESA_WorldCover_v200#bands
  # 10 tree cover
  # 20 shrubland
  # 30 grassland
  # 40 cropland
  # 50 built up
  # 60 bare and sprase vegetation
  # 70 snow and ice
  # 80 permanent water bodies
  # 90 herbaceous wetland
  # 95 mangroves
  # 100 moss and lichen
  
  last_rgrw <- function(i) {
    # function to find the last regrowth
    # i (numerical, 1:17) is the band number
    
    if (!is.na(i[[17]]) & (i[[17]] %in% c(20, 30, 40, 50, 60, 70, 80, 90, 95, 100))) {0} # non forest areas
    else if (!is.na(i[[15]])) {i[[15]]} # regrowth
    else if (!is.na(i[[13]])) {0} # disturbance
    else if (!is.na(i[[11]])) {i[[11]]} # regrowth
    else if (!is.na(i[[9]])) {0} # disturbance
    else if (!is.na(i[[7]])) {i[[7]]} # regrowth
    else if (!is.na(i[[5]])) {0} # disturbance
    else if (!is.na(i[[3]])) {i[[3]]} # regrowth 
    else if (!is.na(i[[1]])) {0} # disturbance
    else if (is.na(i[[1]])) {1} # regrowth (old growth)
    else {NA}
  }
  
  # apply the last_rgrw function to the the distreg raster and save it in a new band (band 18)
  distreg$lyr.18 <- app(distreg, last_rgrw)
  
  distreg <- crop(distreg, aoi)
  distreg <- mask(distreg, aoi)
  
  # save the new raster as a new .tif file in the data folder
  writeRaster(distreg, file.path(data_folder, "10_forest_mosaic.tif"), datatype = "FLT4S", overwrite=TRUE)
  
}