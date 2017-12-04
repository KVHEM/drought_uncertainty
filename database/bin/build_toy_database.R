library(dplyr)
library(ggplot2)
library(data.table)

setwd("./database/")
data_path <- "../data/"

mhm_all <- readRDS(file = paste0(data_path, "mhm_all.Rds"))

#Build data tables
temporal <- mhm_all[, .(year, month)]
temporal <- unique(temporal)
temporal[, id_t := 1:nrow(temporal)]

spatial <- mhm_all[, .(lat_y, lon_y)]
spatial <- unique(spatial)
spatial[, id_sp := 1:nrow(spatial)]

mhm_input <- mhm_all[, .(year, month, lon_y, lat_y, pet, pre)]
mhm_input <- merge(temporal, mhm_input,  by = c("year", "month"))
mhm_input <- merge(spatial, mhm_input,  by = c("lon_y", "lat_y"))
mhm_input[, lon_y := NULL]
mhm_input[, lat_y := NULL]
mhm_input[, year := NULL]
mhm_input[, month := NULL]

mhm_output <- mhm_all[, .(year, month, lon_y, lat_y, q, sm)]
mhm_output <- merge(temporal, mhm_output,  by = c("year", "month"))
mhm_output <- merge(spatial, mhm_output,  by = c("lon_y", "lat_y"))
mhm_output[, lon_y := NULL]
mhm_output[, lat_y := NULL]
mhm_output[, year := NULL]
mhm_output[, month := NULL]

#Upload data tables to data base as tables

mhm_database <- src_sqlite("mhm_drought", create = TRUE) 
copy_to(mhm_database, temporal, temporary = FALSE)        # uploading temporal data
copy_to(mhm_database, spatial, temporary = FALSE)       # uploading spatial data
copy_to(mhm_database, mhm_input, temporary = FALSE)      # uploading model input data
copy_to(mhm_database, mhm_output, temporary = FALSE)     # uploading model output data

src_tbls(mhm_database)

