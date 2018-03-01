library(dplyr)
library(ggplot2)
library(data.table)

path_data_storage = "C:/Users/markonis/Documents/Data/MHM/database/"

mhm_db <- src_sqlite(path = paste0(path_data_storage, "mhm_db"))
src_tbls(mhm_db)

db_vars <- tbl(mhm_db, "vars")

random_point <- filter(db_vars, sp_id == '1111')  # Get
explain(random_point)

db_temp <- tbl(mhm_database, "temp")

db_spat <- tbl(mhm_database, "spat")

db_all <- db_vars %>% inner_join(db_temp, by = 'temp_id', copy = TRUE) %>%
  inner_join(db_spat, by= 'sp_id', copy = TRUE) 

random_point <- filter(mhm_output_all, id_sp == '1111')  

mhm_output_all %>% filter(id_sp == '112') %>% 
  group_by(month = month) %>% #grouped by month
  summarize(total = n()) %>%  
  arrange(desc(total)) %>% 
  head(20)

mhm_input %>% group_by(time = occr_country) %>% 
  summarize(Total=n()) %>%      
  arrange(desc(Total))%>%       
  filter(Country!='')%>% head(10)
