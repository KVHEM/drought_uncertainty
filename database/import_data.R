library(raster)
library(data.table)

force2geo = function(force, mhm){
  
  force = flip(force, direction = 'y')
  xmin(force) <- xmin(mhm)
  xmax(force) <- xmax(mhm)
  ymax(force) <- ymax(mhm)
  ymin(force) <- ymin(mhm)
  
  force = mask(force, mhm)
  
}

rb = readRDS('./mha/geo/ccm-regs.rds')    
reg = readRDS('./mha/geo/srex-regs.rds')  
mhm = raster('./mha/geo/mHM_grid.tif') 

##################################################### Precipitation
##### Filenames and locations
filename = "casty_cru_pre_mhm.nc"
data_path = "../data/"

##### Import as raster
casty_P_ras = brick(x = paste0(data_path, filename))
casty_P_ras = force2geo(casty_P_ras,mhm)
resolution = 48000

europe_lat = seq(casty_P_ras@extent@ymin, casty_P_ras@extent@ymax, resolution)
europe_lon = seq(casty_P_ras@extent@xmin, casty_P_ras@extent@xmax, resolution)
europe_grid = expand.grid(europe_lon, europe_lat)

casty_P_dt = t(extract(casty_P_ras, SpatialPoints(europe_grid)))
dim(casty_P_dt)

nnames = apply(europe_grid, 1, paste, collapse = " ")
colnames(casty_P_dt) = nnames

##### Transform to data.table format
casty_P_dt = data.table(melt(casty_P_dt))
casty_P_dt = casty_P_dt[complete.cases(casty_P_dt)]
casty_P_dt[, year := as.numeric(substr(Var1, 2, 5))]
casty_P_dt[, month := as.numeric(substr(Var1, 7, 8))]
casty_P_dt[, Var2 := as.character(Var2)]
casty_P_dt[, lon := as.numeric(sapply(strsplit(Var2,split=" ") , "[[", 1))]
casty_P_dt[, lat := as.numeric(sapply(strsplit(Var2,split=" ") , "[[", 2))]
casty_P_dt[, Var1 := NULL]
casty_P_dt[, Var2 := NULL]
setcolorder(casty_P_dt, c("year", "month", "lon", "lat", "value")) 
colnames(casty_P_dt)[5] = "precip"

#plot(casty_P_dt)

##################################################### PET
##### Filenames and locations
filename = "casty_cru_pet_mhm.nc"

##### Import as raster
casty_PET_ras = brick(x = paste0(data_path, filename))
casty_PET_ras = force2geo(casty_PET_ras,mhm)
# resolution = 48000

europe_lat = seq(casty_P_ras@extent@ymin, casty_P_ras@extent@ymax, resolution)
europe_lon = seq(casty_P_ras@extent@xmin, casty_P_ras@extent@xmax, resolution)
europe_grid = expand.grid(europe_lon, europe_lat)

casty_PET_dt = t(extract(casty_PET_ras, SpatialPoints(europe_grid)))

nnames = apply(europe_grid, 1, paste, collapse = " ")
colnames(casty_PET_dt) = nnames

##### Transform to data.table format
casty_PET_dt = data.table(melt(casty_PET_dt))
casty_PET_dt = casty_PET_dt[complete.cases(casty_PET_dt)]
casty_PET_dt[, year := as.numeric(substr(Var1, 2, 5))]
casty_PET_dt[, month := as.numeric(substr(Var1, 7, 8))]
casty_PET_dt[, Var2 := as.character(Var2)]
casty_PET_dt[, lon := as.numeric(sapply(strsplit(Var2,split=" ") , "[[", 1))]
casty_PET_dt[, lat := as.numeric(sapply(strsplit(Var2,split=" ") , "[[", 2))]
casty_PET_dt[, Var1 := NULL]
casty_PET_dt[, Var2 := NULL]
setcolorder(casty_PET_dt, c("year", "month", "lon", "lat", "value")) 
colnames(casty_PET_dt)[5] = "PET"

casty_PET_dt[, sum(PET),by=list(year,lon,lat)]

casty_PET_dt[, a_PET:=sum(PET),by=list(year,lon,lat)]
casty_P_dt[, a_precip:=sum(precip),by=list(year,lon,lat)]




