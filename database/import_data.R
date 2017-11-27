library(raster)
library(data.table)

setwd("~/sync/Various_Proj/drought_uncertainty/database") ## Local path where

force2geo <- function(force, mhm){
  
  force = flip(force, direction = 'y')
  xmin(force) <- xmin(mhm)
  xmax(force) <- xmax(mhm)
  ymax(force) <- ymax(mhm)
  ymin(force) <- ymin(mhm)
  
  force <- mask(force, mhm)
}

rb <- readRDS('./database/mha/geo/ccm-regs.rds')    
reg <- readRDS('./database/mha/geo/srex-regs.rds')  
mhm <- raster('./database/mha/geo/mHM_grid.tif') 

##################################################### Precipitation
##### Filenames and locations
filename <- "casty_cru_pre_mhm.nc"
data_path <- "./data/"
resolution <- 48000

##### Import as raster
casty_pre_ras <- brick(x = paste0(data_path, filename))
casty_pre_ras <- force2geo(casty_pre_ras,mhm)

europe_lat_y <- seq(casty_pre_ras@extent@ymin, casty_pre_ras@extent@ymax, resolution)
europe_lon_y <- seq(casty_pre_ras@extent@xmin, casty_pre_ras@extent@xmax, resolution)
europe_grid <- expand.grid(europe_lon_y, europe_lat_y)

casty_pre_dt <- t(extract(casty_pre_ras, SpatialPoints(europe_grid)))
dim(casty_pre_dt)

nnames <- apply(europe_grid, 1, paste, collapse = " ")
colnames(casty_pre_dt) = nnames

##### Transform to data.table format
casty_pre_dt = data.table(melt(casty_pre_dt))
casty_pre_dt = casty_pre_dt[complete.cases(casty_pre_dt)]
casty_pre_dt[, year := as.numeric(substr(Var1, 2, 5))]
casty_pre_dt[, month := as.numeric(substr(Var1, 7, 8))]
casty_pre_dt[, Var2 := as.character(Var2)]
casty_pre_dt[, lon_y := as.numeric(sapply(strsplit(Var2, split = " ") , "[[", 1))]
casty_pre_dt[, lat_y := as.numeric(sapply(strsplit(Var2, split = " ") , "[[", 2))]
casty_pre_dt[, Var1 := NULL]
casty_pre_dt[, Var2 := NULL]
setcolorder(casty_pre_dt, c("year", "month", "lon_y", "lat_y", "value")) 
colnames(casty_pre_dt)[5] = "pre"

##################################################### PET
##### Filenames and locations
filename = "casty_cru_pet_mhm.nc"

##### Import as raster
casty_pet_ras <- brick(x = paste0(data_path, filename))
casty_pet_ras <- force2geo(casty_pet_ras,mhm)

casty_pet_dt <- t(extract(casty_pet_ras, SpatialPoints(europe_grid)))

nnames <- apply(europe_grid, 1, paste, collapse = " ")
colnames(casty_pet_dt) = nnames

##### Transform to data.table format
casty_pet_dt <- data.table(melt(casty_pet_dt))
casty_pet_dt <- casty_pet_dt[complete.cases(casty_pet_dt)]
casty_pet_dt[, year := as.numeric(substr(Var1, 2, 5))]
casty_pet_dt[, month := as.numeric(substr(Var1, 7, 8))]
casty_pet_dt[, Var2 := as.character(Var2)]
casty_pet_dt[, lon_y := as.numeric(sapply(strsplit(Var2,split=" ") , "[[", 1))]
casty_pet_dt[, lat_y := as.numeric(sapply(strsplit(Var2,split=" ") , "[[", 2))]
casty_pet_dt[, Var1 := NULL]
casty_pet_dt[, Var2 := NULL]
setcolorder(casty_pet_dt, c("year", "month", "lon_y", "lat_y", "value")) 
colnames(casty_pet_dt)[5] = "pet"







