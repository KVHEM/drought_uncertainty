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
temp[, dtm := NULL]
temp[, temp_id := 1:nrow(temp)]

vars <- alldata[, .(var, value)]
p_var <- vars[var %in% 'p'] 
pet_var <- vars[var %in% 'pet']
vars <- data.table(cbind(pre = p_var$value, pet = pet_var$value))
vars[, vars_id := 1:nrow(vars)]

mhm <- alldata
mhm <- unique(mhm)
mhm[, mhm_id := 1:nrow(mhm)]

mhm <- merge(mhm, temp, by = c("year", "month"))
mhm <- merge(mhm, spat, by = c("lat", "lon"))

mhm[, ens_id := 1]   # create ens table 
mhm <- mhm[ , .(ens_id, mhm_id, sp_id, temp_id, pet, pre, sm, q)]


my_database <- src_sqlite(path = "mhm_db", create = TRUE)

copy_to(my_database, demography, temporary = FALSE)





