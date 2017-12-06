force2geo = function(force, mhm){
  
  force = flip(force, direction = 'y')
  xmin(force) <- xmin(mhm)
  xmax(force) <- xmax(mhm)
  ymax(force) <- ymax(mhm)
  ymin(force) <- ymin(mhm)
  
  force = mask(force, mhm)
  
}

require(raster)
require(data.table)

setwd("./database/")

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

setwd('./output/')
q = brick('mHM_Fluxes_States_ncl_d4.nc', varname = 'Q')   # loop marker
s = brick('mHM_Fluxes_States_ncl_d4.nc', varname = 'SM')   # loop marker

dta = list() 
for (ii in 1:5){
  dt = as.array(list(p, q, s, t, pt)[[ii]]) # convert raster to array 
  dimnames(dt) = list(y = 1:nrow(q), x = 1:ncol(q), dtm = as.character(osa))
  dta[[ii]] = data.table(var = c('p', 'q', 's', 't', 'pet')[ii], melt(dt))[!is.na(value)] # convert to data.table
}

dta = rbindlist(dta) 
dta[, dtm := NULL] # it is not date, conversion from factor to IDate is extremely slow
dta[, dtm := osa, by = .(var, y, x)] # better to replace - was checked
dta = dta[!is.na(value)]

xy = dta[dtm==dtm[1], ][, unique(paste(x, y)), by = var]
excl = dcast.data.table(xy, V1 ~ var)[is.na(t)]
dta = dta[!paste(x, y) %in% excl$V1, ]

dta = reg[dta, on = c('x', 'y')] # add regions
dta = rb[dta, on = c('x', 'y')]

#dta[, obd:= cut(dtm, breaks = '30 year'), by = .(x, y, var)]

dta[, lon := xFromCol(mhm, x)]
dta[, lat := yFromRow(mhm, y)]

setwd("../../../data/")

dta[, y := NULL]
dta[, x := NULL]
dta[, CCM_REG := NULL]

saveRDS(dta, file = "./mhm_all.Rds")
