library(dplyr)
library(ggplot2)
library(data.table)

path_data_storage = "C:/Users/markonis/Documents/Data/MHM/database/"

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

test <- tbl(mhm_db , "vars")
random_point <- filter(test, sp_id == '1111')
random_point %>% head(20)
random_point %>% group_by(ens_id) %>% summarize(Total=n())
