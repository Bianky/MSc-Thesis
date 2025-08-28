compute_ff <- function(data_folder){
  # function to compute the forest factors
  # data_folder is the folder where the data is stored
  
  # forest cover plots coordinates
  fc_plots <- read.csv(file.path(data_folder, "01_MX_FC_coords_EPSG4326.csv"))
  
  plots <- st_as_sf(fc_plots, coords = c("Long", "Lat"), crs = 4326) # convert points to sf object 
  
  plots <- st_transform(plots, crs = 6372) # reproject to a crs with m unit
  
  plots$buffer <- st_buffer(plots$geometry, dist = 1000, endCapStyle = 'ROUND') # create a research unit consisting of points and their buffer
  
  # load in forest mosaic
  forest_mosaic <- rast(file.path(data_folder, "10_forest_mosaic.tif"))[[18]]
  
  forest_mosaic <- project(forest_mosaic, 'epsg:6372', method = "near") # reproject to a crs with m unit
  
  vals <- terra::values(forest_mosaic) # extract all values
  
  # change values to be larger than 100 so they are certainly not mistaken for forest age (calculated in the next step)
  vals[vals == 0] <- 100 # set non forest areas to have a value of 100
  vals[vals == 1] <- 101 # set never deforested areas to have a value of 101
  
  # if value is not 100, 101, subtract it from 2022 to calculate forest age
  vals[!(vals %in% c(100, 101))] <- 2022 - vals[!(vals %in% c(100, 101))]
  
  vals[vals < 30 | vals == 30 ] <- 1
  vals[vals == 101] <- 2 # never deforested area
  vals[vals == 100] <- 0 # non forest areas
  
  values(forest_mosaic) <- vals

  rasters_list <- list()
  
  # create a separate raster for each research unit
  for (i in 1:nrow(plots)) {
    current_buffer <- vect(plots$buffer[i, ])
    cropped_raster <- crop(forest_mosaic, current_buffer)
    cropped_raster <- mask(cropped_raster, current_buffer)
    
    ID <- plots$ID[i]
    rasters_list[[as.character(ID)]] <- cropped_raster
  }
  
  # calculate the percentage of each age
  perc_age <- lapply(names(rasters_list), function(name) {
    rast <- rasters_list[[name]]
    freq_tb <- freq(rast)
    total <- sum(freq_tb[freq_tb[, "value"] != 0, "count"])
    
    freq_tb$percent <- ifelse(freq_tb[, "value"] != 0,
                              freq_tb[, "count"] / total ,
                              0)
    
    freq_tb <- freq_tb %>%
      dplyr::select(-count, -layer) %>%
      pivot_wider(names_from = value, values_from = percent) %>% 
      rename(early = `1`, 
             late = `2`)
    
    # add raster name as plot id
    freq_tb$plot_id <- name
    
    return(freq_tb)
  })
  
  age_percentage <- bind_rows(perc_age)
  
  # calculate the mean age of each research unit
  mean_age <- sapply(rasters_list, function(rast) {
    rast[rast == 0] <- NA
    global(rast, fun = "mean", na.rm = TRUE)[1,1]
  })
  
  # store the mean age per research unit in a table format
  age <- tibble(
    plot_id = names(mean_age),
    mean_age = round(as.numeric(mean_age), 0)
  )
  
  # set all forest values to 1 and non-forest values to 0
  vals <- terra::values(forest_mosaic) # extract all values
  vals[vals > 0] <- 1
  values(forest_mosaic) <- vals
  
  # calculate forest area
  area <- sample_lsm(forest_mosaic, plots$geometry, plot_id = plots$ID, shape = "circle", size = 1000, directions = 8, what = c("lsm_c_ca", "lsm_c_pland")) %>% 
    filter(class == 1) %>% 
    pivot_wider(names_from = metric, values_from = value) %>% 
    mutate(pland = pland/100)
  
  # calculate forest connectivity
  connectivity <- sample_lsm(forest_mosaic, plots$geometry, plot_id = plots$ID, shape = "circle", size = 1000, directions = 8, what =  c("lsm_c_enn_mn", "lsm_c_np")) %>% 
    filter(class == 1) %>% 
    pivot_wider(names_from = metric, values_from = value) %>% 
    mutate(enn_mn = replace_na(enn_mn, 0), 
           total_enn = enn_mn * np, 
           enn_mn_inv = 1/enn_mn)
  
  # merge all variables together
  age_area <- full_join(age, area)
  age_area_perc <- full_join(age_area, age_percentage)
  forest_factors <- full_join(age_area_perc, connectivity) %>% 
    dplyr::select(plot_id, mean_age, percentage_inside, ca, pland, np, enn_mn, enn_mn_inv, total_enn, early, late) %>% 
    rename(ID = plot_id, 
           forest_cover = pland,
           forest_connectivity = enn_mn_inv,
           forest_early_ss = early,
           forest_late_ss = late
           ) %>% 
    mutate(forest_type = ifelse(grepl("DR", ID), "dry", "wet"))
    
  
  write.csv(forest_factors, file.path(data_folder, "11_forest_factors.csv"))
  
}
