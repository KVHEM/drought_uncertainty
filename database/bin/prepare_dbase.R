library(dplyr)
library(ggplot2)
library(data.table)

path_data_storage = "C:/Users/markonis/Documents/Data/MHM/database/"
alldata <- readRDS("./database/data/mhm_all.Rds")

mhm_db <- src_sqlite(path = paste0(path_data_storage, "mhm_db"), create = TRUE)

for(par_set in 1:10){ #ens_par loop
  for(met_set in 1:10){ #ens_met loop
    ens_counter <- met_set + (par_set - 1) * 10
    print(paste0(ens_counter, "/100"))
    vars <- readRDS(
      paste0(path_data_storage, "vars_dt_par", par_set, "_met", met_set, ".Rds"))
    db_insert_into(mhm_db$con, table = "vars", values = vars)
  } #ens_met loop
} #ens_par loop

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
vars <- vars[, .(sp_id, temp_id, var, value)]
vars <- dcast(vars, sp_id + temp_id ~ var, value.var = "value")
vars[, vars_id := 1:nrow(vars)] 
vars[, ens_id := 1]                              

ensemble <- data.table(expand.grid(1:10, 1:10))
ensemble[, ens_id := 1:nrow(ensemble)]
colnames(ensemble)[1:2] <- c('met', 'par')
saveRDS(vars, "./database/results/vars_dt.rds")

my_database <- src_sqlite(path = "mhm_db", create = TRUE)

copy_to(my_database, spat, temporary = FALSE)
copy_to(my_database, temp, temporary = FALSE)
copy_to(my_database, ensemble, temporary = FALSE)
