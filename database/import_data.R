library(raster)
library(data.table)

getwd() ## Local path where the project lies

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

casty_pre <- t(extract(casty_pre_ras, SpatialPoints(europe_grid)))
dim(casty_pre)

nnames <- apply(europe_grid, 1, paste, collapse = " ")
colnames(casty_pre) = nnames

##### Transform to data.table format
casty_pre = data.table(melt(casty_pre))
casty_pre = casty_pre[complete.cases(casty_pre)]
casty_pre[, year := as.numeric(substr(Var1, 2, 5))]
casty_pre[, month := as.numeric(substr(Var1, 7, 8))]
casty_pre[, Var2 := as.character(Var2)]
casty_pre[, lon_y := as.numeric(sapply(strsplit(Var2, split = " ") , "[[", 1))]
casty_pre[, lat_y := as.numeric(sapply(strsplit(Var2, split = " ") , "[[", 2))]
casty_pre[, Var1 := NULL]
casty_pre[, Var2 := NULL]
setcolorder(casty_pre, c("year", "month", "lon_y", "lat_y", "value")) 
colnames(casty_pre)[5] = "pre"

##################################################### PET
##### Filenames 
filename = "casty_cru_pet_mhm.nc"

##### Import as raster
casty_pet_ras <- brick(x = paste0(data_path, filename))
casty_pet_ras <- force2geo(casty_pet_ras,mhm)

casty_pet <- t(extract(casty_pet_ras, SpatialPoints(europe_grid)))
colnames(casty_pet) = nnames

##### Transform to data.table format
casty_pet <- data.table(melt(casty_pet))
casty_pet <- casty_pet[complete.cases(casty_pet)]
casty_pet[, year := as.numeric(substr(Var1, 2, 5))]
casty_pet[, month := as.numeric(substr(Var1, 7, 8))]
casty_pet[, Var2 := as.character(Var2)]
casty_pet[, lon_y := as.numeric(sapply(strsplit(Var2,split=" ") , "[[", 1))]
casty_pet[, lat_y := as.numeric(sapply(strsplit(Var2,split=" ") , "[[", 2))]
casty_pet[, Var1 := NULL]
casty_pet[, Var2 := NULL]
setcolorder(casty_pet, c("year", "month", "lon_y", "lat_y", "value")) 
colnames(casty_pet)[5] = "pet"

mhm_input = merge(casty_pet_dt, casty_pre_dt)

##################################################### Output: Runoff
filename = "output/mHM_Fluxes_States_ncl_d4.nc"
mhm_q_ras <- brick(x = paste0(data_path, filename), varname = "Q")

mhm_q <- t(extract(mhm_q_ras, SpatialPoints(europe_grid)))
colnames(mhm_q) = nnames

mhm_q <- data.table(melt(mhm_q))
mhm_q <- mhm_q[complete.cases(mhm_q)]
mhm_q[, year := as.numeric(substr(Var1, 2, 5))]
mhm_q[, month := as.numeric(substr(Var1, 7, 8))]
mhm_q[, Var2 := as.character(Var2)]
mhm_q[, lon_y := as.numeric(sapply(strsplit(Var2,split=" ") , "[[", 1))]
mhm_q[, lat_y := as.numeric(sapply(strsplit(Var2,split=" ") , "[[", 2))]
mhm_q[, Var1 := NULL]
mhm_q[, Var2 := NULL]
setcolorder(mhm_q, c("year", "month", "lon_y", "lat_y", "value")) 
colnames(mhm_q)[5] = "q"

##################################################### Output: Soilmoisture
mhm_sm_ras <- brick(x = paste0(data_path, filename), varname = "SM")
mhm_sm <- t(extract(mhm_sm_ras, SpatialPoints(europe_grid)))
colnames(mhm_sm) = nnames

mhm_sm <- data.table(melt(mhm_sm))
mhm_sm <- mhm_sm[complete.cases(mhm_sm)]
mhm_sm[, year := as.numeric(substr(Var1, 2, 5))]
mhm_sm[, month := as.numeric(substr(Var1, 7, 8))]
mhm_sm[, Var2 := as.character(Var2)]
mhm_sm[, lon_y := as.numeric(sapply(strsplit(Var2,split=" ") , "[[", 1))]
mhm_sm[, lat_y := as.numeric(sapply(strsplit(Var2,split=" ") , "[[", 2))]
mhm_sm[, Var1 := NULL]
mhm_sm[, Var2 := NULL]
setcolorder(mhm_sm, c("year", "month", "lon_y", "lat_y", "value")) 
colnames(mhm_sm)[5] = "sm"

mhm_output = merge(mhm_sm, mhm_q)

###################################################### Save Files
saveRDS(casty_pet, file = paste0(data_path, "casty_cru_pet_mhm.Rds"))
saveRDS(casty_pre, paste0(data_path, "casty_cru_pre_mhm.Rds"))
saveRDS(mhm_output, file = paste0(data_path, "mhm_output.Rds"))
saveRDS(mhm_input, file = paste0(data_path, "mhm_input.Rds"))
saveRDS(merge(mhm_input,mhm_output),  file = paste0(data_path, "mhm_all.Rds"))





