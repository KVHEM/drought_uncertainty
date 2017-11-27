force2geo = function(force, mhm){
  
  force = flip(force, direction = 'y')
  xmin(force) <- xmin(mhm)
  xmax(force) <- xmax(mhm)
  ymax(force) <- ymax(mhm)
  ymin(force) <- ymin(mhm)
  
  force = mask(force, mhm)
  
}

setwd('./mha/')
rb = readRDS('geo/ccm-regs.rds')   # geodata - evropske oblasti povodi
reg = readRDS('geo/srex-regs.rds') # IPCC oblasti
mhm = raster('geo/mHM_grid.tif') 


setwd('/home/owc/PaleoHydroEU/data_preparation/forcings/map_meteo_to_mhm_grid/')
p = brick('casty_cru_pre_mhm.nc') # nacti srazky
pok=force2geo(casty_P_ras,mhm)
p = force2geo(p, mhm)

t = brick('casty_cru_tavg_mhm.nc') # nacti teplotu
t = force2geo(t, mhm)

pt = brick('casty_cru_oudin_pet_mhm.nc') # nacti pet
pt = force2geo(pt, mhm)

setwd('/home/owc/PaleoHydroEU/results/mHM/output_toKVHEM/par_001/met_001/output/')
q = brick('mHM_Fluxes_States_ncl_d4.nc', varname = 'Q')
s = brick('mHM_Fluxes_States_ncl_d4.nc', varname = 'SM')

osa = as.IDate(seq.Date(from = as.Date('1766-01-01'), to = as.Date('2015-12-01'), by = 'month'))

dta = list() 
for (ii in 1:5){
  
  dt = as.array(list(p, q, s, t, pt)[[ii]]) # convert raster to array 
  dimnames(dt) = list(y = 1:nrow(q), x = 1:ncol(q), DTM = as.character(osa))
  dta[[ii]] = data.table(var = c('p', 'q', 's', 't', 'pet')[ii], melt(dt))[!is.na(value)] # convert to data.table
  
}

dta = rbindlist(dta) 
dta[, DTM := NULL] # it is not date, conversion from factor to IDate is extremely slow
dta[, DTM := osa, by = .(var, y, x)] # better to replace - was checked
dta = dta[!is.na(value)]

xy = dta[DTM==DTM[1], ][, unique(paste(x, y)), by = var]
excl = dcast.data.table(xy, V1 ~ var)[is.na(t)]
dta = dta[!paste(x, y) %in% excl$V1, ]

dta = reg[dta, on = c('x', 'y')] # add regions
dta = rb[dta, on = c('x', 'y')]

dta[, obd:= cut(DTM, breaks = '30 year'), by = .(x, y, var)]
