library(dplyr)
library(ggplot2)
library(data.table)

alldata <- readRDS("./data/mhm_all.Rds")

spat <- alldata[, .(lat, lon, CCM_LAB, REG)]
spat <- unique(spat)
spat[, sp_id := 1:nrow(spat)]

temp <- alldata[, .(dtm)]
temp <- unique(temp)
temp[, month := month(dtm)]
temp[, year := year(dtm)]
temp[, temp_id := 1:nrow(temp)]

vars <- merge(alldata, temp, by = "dtm")
vars <- merge(vars, spat, by = c("lat", "lon"))
vars = vars[, .(sp_id, temp_id, var, value)]
vars = dcast(vars, sp_id + temp_id ~ var, value.var = "value")
vars[, vars_id := 1:nrow(vars)] 
vars[, ens_id := 1]   

my_database <- src_sqlite(path = "mhm_db", create = TRUE)

copy_to(my_database, demography, temporary = FALSE)





