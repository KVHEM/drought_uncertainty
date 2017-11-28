library(dplyr)
library(ggplot2)
library(data.table)

setwd("./database/")
data_path <- "../data/"

mhm_database <- src_sqlite("mhm_drought", create = TRUE) 

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

copy_to(mhm_database, temporal, temporary = FALSE)        # uploading temporal data
copy_to(mhm_database, spatial, temporary = FALSE)       # uploading spatial data
copy_to(mhm_database, mhm_input, temporary = FALSE)      # uploading model input data
copy_to(mhm_database, mhm_output, temporary = FALSE)     # uploading model output data

src_tbls(mhm_database)

demography = tbl(my_db,"demography" )

US = filter(demography, occr_country=='US')  # Filtering demography of patients from the US
explain(US)

drug = tbl(my_db,"drug" )
indication = tbl(my_db,"indication" )
outcome = tbl(my_db,"outcome" )
reaction = tbl(my_db,"reaction" )

head(indication,3)

demography%>%group_by(Country= occr_country)%>% 
  summarize(Total=n())%>%      
  arrange(desc(Total))%>%       
  filter(Country!='')%>% head(10)

demography%>%group_by(Country= occr_country)%>% #grouped by country
  summarize(Total=n())%>%    # found the count for each country
  arrange(desc(Total))%>%    # sorted them in descending order
  filter(Country!='')%>%     # removed reports that does not have country information
  head(10)%>%                  # took the top ten
  ggplot(aes(x=Country,y=Total))+geom_bar(stat='identity',color='skyblue',fill='#b35900')+
  xlab("")+ggtitle('Top ten countries with highest number of adverse event reports')+
  coord_flip()+ylab('Total number of reports')

drug%>%group_by(drug_name= drugname)%>% #grouped by drug_name
  summarize(Total=n())%>%    # found the count for each drug name
  arrange(desc(Total))%>%    # sorted them in descending order
  head(1)                   # took the most frequent drug

head(outcome,3)  # to see the variable names
head(reaction,3)  # to see the variable names

reaction%>%group_by(reactions= pt)%>% # grouped by reactions
  summarize(Total=n())%>%    # found the count for each reaction type
  arrange(desc(Total))%>%    # sorted them in descending order
  head(10)                   # took the top ten

inner_joined = demography%>%inner_join(outcome, by='primaryid',copy = TRUE)%>%
  inner_join(reaction, by='primaryid',copy = TRUE)

head(inner_joined)

drug_indication= indication%>%rename(drug_seq=indi_drug_seq)%>%
  inner_join(drug, by=c("primaryid","drug_seq"))

head(drug_indication)
