avocado <- function(data_folder) {
  # function to run the avocado algorithm to detect a sequence of disturbances and regrowths in the aoi
  # Decuyper, M., Chávez, R. O., Lohbeck, M., Lastra, J. A., Tsendbazar, N., Hackländer, J., ... & Vågen, T. G. (2022). Continuous monitoring of forest change dynamics with satellite time series. Remote Sensing of Environment, 269, 112829.

  # data_folder is a folder where data is stored
  
  # load satellite imagery
  NDMIbrick <- rast(file.path(data_folder, "05_satellite/Landsat_NDMI_1992-01-01_2022-12-31.tif")) 
  # load dates of satellite imagery scenes
  dates.table <- read.csv(file.path(data_folder, "05_satellite/Date_Landsat_NDMI_1992-01-01_2022-12-31.csv")) %>% 
    mutate(Date = gsub("NDMI_", "", Date))
  NDMIbrick_dates <- as.Date(dates.table$Date, format = '%Y%m%d')
  
  # load reference polygon of a mature forest
  ref.shp <- vect(file.path(data_folder, "03_ref.shp"))
  
  ref.ext <- ext(ref.shp) # get extent
  ref.brick <- crop(NDMIbrick, ref.ext) # crop to the reference area
  
  fin <- nrow(ref.brick) * ncol(ref.brick) # total number of pxls in the cropped raster
  phen <- as.numeric(terra::extract(ref.brick, 1)) 
  
  d1 <- NDMIbrick_dates # time stamps for all layers
  
  for (i in 2:fin) {
    pp <- as.numeric(terra::extract(ref.brick, i)) # extract time series for one pixels
    phen <- c(phen, pp) # append to the full phenology vector
    d1 <- c(d1, NDMIbrick_dates) # append the corresponding dates
  }
  
  # extraction of reference curve and likelihoods
  NDMIbrick_fref <- PhenRef2d(phen,
                              d1,
                              h = 1,
                              anop = c(1:nlyr(NDMIbrick)),
                              rge = c(0, 10000))
  
  # plotting reference curve + probabilities 
  png(file.path(data_folder, "06_rephen_plot.png"), width = 800, height = 600)
  
  PhenKplot(x = phen, d1, h = 1, xlab = "DOY", ylab = "NDMI", rge = c(-3000,7000))
  
  dev.off()
  
  # WALL TO WALL OUTPUT
  # anomaly calculation and their likelihoods for wall to wall output
  library(parallel)
  library(lubridate)
  
  # need for a high computational power
  
  #ncl <- detectCores() - 2
  
  #PLUGPhenAnoRFDMapPLUS(s = NDMIbrick, dates = NDMIbrick_dates, h = 1, phenref = NDMIbrick_fref, anop = c(1:nlyr(NDMIbrick)), nCluster = ncl, outname = file.path(data_folder, '07_landsat_1992_2022_AnomProb.tif'), datatype = 'INT2S')
  
  # loading of anomaly and likelihoods data
  #ano.rfd.st <- rast(file.path(data_folder, "07_landsat_1992_2022_AnomProb.tif")) # example to load your own data
  
  # disturbance computation
  #dist.reg.map(
  #  s = ano.rfd.st, dates = NDMIbrick_dates, rfd = 0.99, dstrb_thr = 730, rgrow_thr = 365, nCluster = ncl,
  #  cdates = 3, outname = file.path(data_folder, "08_landsat_1992_2022_DistReg.tif"), datatype = 'INT2S')
  # distrb_thr = 730 to avoid for drought disturbance detection such as el nino impacts
  # rgrow_thr = 365 to avoid for regrowth detection of annually grown crops
  # rfd = 99 to have high certainty to detect deforestation disturbances only
}
