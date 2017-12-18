require(raster)
require(data.table)

force2geo = function(force, mhm){
  
  force = flip(force, direction = 'y')
  xmin(force) <- xmin(mhm)
  xmax(force) <- xmax(mhm)
  ymax(force) <- ymax(mhm)
  ymin(force) <- ymin(mhm)
  
  force = mask(force, mhm)
  
}

alldata <- readRDS("./data/mhm_all.Rds")

path_data_storage = "C:/Users/markonis/Documents/Data/MHM/mhm_out/"
ens_par_dirs = list.files(path_data_storage)
ens_met_dirs = list.files(paste0(path_data_storage, ens_par_dirs[1]))

rb = readRDS('./data/geo/ccm-regs.rds')   # geodata - evropske oblasti povodi
reg = readRDS('./data/geo/srex-regs.rds') # IPCC oblasti
mhm = raster('./data/geo/mHM_grid.tif') 

setwd('./data/raw_data/')
p = brick('casty_cru_pre_mhm.nc') # nacti srazky
p = force2geo(p, mhm)

t = brick('casty_cru_tavg_mhm.nc') # nacti teplotu
t = force2geo(t, mhm)

pt = brick('casty_cru_pet_mhm.nc') # nacti pet
pt = force2geo(pt, mhm)

osa = as.IDate(seq.Date(from = as.Date('1766-01-01'), to = as.Date('2015-12-01'), by = 'month'))

spat <- alldata[, .(lat, lon, CCM_LAB, REG)]
spat <- unique(spat)
spat[, sp_id := 1:nrow(spat)]

temp <- alldata[, .(dtm)]
temp <- unique(temp)
temp[, month := month(dtm)]
temp[, year := year(dtm)]
temp[, temp_id := 1:nrow(temp)]

ensemble <- data.table(expand.grid(1:10, 1:10))
ensemble[, ens_id := 1:nrow(ensemble)]
colnames(ensemble)[1:2] <- c('met', 'par')

for(par_set in 1:10){ #ens_par loop
  for(met_set in 1:10){ #ens_met loop
    ens_counter <- met_set + (par_set - 1) * 10
    print(paste0(ens_counter, "/100"))
    q <- brick(paste0(path_data_storage, ens_par_dirs[par_set], "/", ens_met_dirs[met_set], '/output/mHM_Fluxes_States_ncl_d4.nc'), varname = 'Q')   # loop marker
    s <- brick(paste0(path_data_storage, ens_par_dirs[par_set], "/", ens_met_dirs[met_set], '/output/mHM_Fluxes_States_ncl_d4.nc'), varname = 'SM')   # loop marker
    
    dta = list() 
    for (ii in 1:5){
      dt <- as.array(list(p, q, s, t, pt)[[ii]]) # convert raster to array 
      dimnames(dt) <- list(y = 1:nrow(q), x = 1:ncol(q), dtm = as.character(osa))
      dta[[ii]] <- data.table(var = c('p', 'q', 'sm', 't', 'pet')[ii], melt(dt))[!is.na(value)] # convert to data.table
    }
    
    dta <- rbindlist(dta) 
    dta[, dtm := NULL] # it is not date, conversion from factor to IDate is extremely slow
    dta[, dtm := osa, by = .(var, y, x)] # better to replace - was checked
    dta <- dta[!is.na(value)]
    
    xy <- dta[dtm==dtm[1], ][, unique(paste(x, y)), by = var]
    excl <- dcast.data.table(xy, V1 ~ var)[is.na(t)]
    dta <- dta[!paste(x, y) %in% excl$V1, ]
    
    dta <- reg[dta, on = c('x', 'y')] # add regions
    dta <- rb[dta, on = c('x', 'y')]
    
    dta[, lon := xFromCol(mhm, x)]
    dta[, lat := yFromRow(mhm, y)]
    
    dta[, y := NULL]
    dta[, x := NULL]
    dta[, CCM_REG := NULL]
    
    vars <- merge(dta, temp, by = "dtm")
    vars <- merge(vars, spat, by = c("lat", "lon"))
    vars <- vars[, .(sp_id, temp_id, var, value)]
    vars <- dcast(vars, sp_id + temp_id ~ var, value.var = "value")
    vars[, vars_id := 1:nrow(vars)] 
    vars[, ens_id := ensemble$ens_id[ens_counter]]                              
    
    saveRDS(vars, paste0(path_data_storage, "rds/vars_dt_par", par_set, "_met", met_set, ".Rds"))
  }  #ens_met loop
}  #ens_par loop





