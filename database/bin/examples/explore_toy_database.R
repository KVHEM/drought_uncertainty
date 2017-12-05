library(dplyr)
library(ggplot2)
library(data.table)

setwd("./database/")
data_path <- "../data/"

mhm_database <- src_sqlite("mhm_drought") 
src_tbls(mhm_database)

mhm_output <- tbl(mhm_database, "mhm_output")
gc()
#mhm_output <- as.data.table(mhm_output)
#gc()

random_point <- filter(mhm_output, id_sp == '1111')  # Get
explain(random_point)

mhm_input <- tbl(mhm_database, "mhm_input")
temporal <- tbl(mhm_database, "temporal")
spatial <- tbl(mhm_database, "spatial")

mhm_output_all = mhm_output %>% inner_join(temporal, by = 'id_t', copy = TRUE) %>%
  inner_join(spatial, by= 'id_sp', copy = TRUE) 

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
