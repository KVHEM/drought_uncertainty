library(dplyr)
library(ggplot2)
library(data.table)

year_start=2013
year_last=2015
for (i in year_start:year_last){
  j=c(1:4)
  for (m in j){
    url1<-paste0("http://www.nber.org/fda/faers/",i,"/demo",i,"q",m,".csv.zip")
    download.file(url1,dest="data.zip") # Demography
    unzip ("data.zip")
    url2<-paste0("http://www.nber.org/fda/faers/",i,"/drug",i,"q",m,".csv.zip")
    download.file(url2,dest="data.zip")   # Drug 
    unzip ("data.zip")
    url3<-paste0("http://www.nber.org/fda/faers/",i,"/reac",i,"q",m,".csv.zip")
    download.file(url3,dest="data.zip") # Reaction
    unzip ("data.zip")
    url4<-paste0("http://www.nber.org/fda/faers/",i,"/outc",i,"q",m,".csv.zip")
    download.file(url4,dest="data.zip") # Outcome
    unzip ("data.zip")
    url5<-paste0("http://www.nber.org/fda/faers/",i,"/indi",i,"q",m,".csv.zip")
    download.file(url5,dest="data.zip") # Indication for use
    unzip ("data.zip")
  }
}

filenames <- list.files(pattern="^demo.*.csv", full.names=TRUE)
demo=lapply(filenames,fread)
demography=do.call(rbind,lapply(1:length(demo), function(i) 
  select(as.data.frame(demo[i]),primaryid,caseid, 
         age,age_cod,event_dt,sex,wt,wt_cod, occr_country)))

filenames <- list.files(pattern="^drug.*.csv", full.names=TRUE)
drug_list=lapply(filenames,fread)
drug=do.call(rbind,lapply(1:length(drug_list), function(i) 
  select(as.data.frame(drug_list[i]),primaryid,drug_seq,drugname,route)))

filenames <- list.files(pattern="^indi.*.csv", full.names=TRUE)
indi=lapply(filenames,fread)
indication=do.call(rbind,lapply(1:length(indi), function(i) 
  select(as.data.frame(indi[i]),primaryid,indi_drug_seq,indi_pt)))

filenames <- list.files(pattern="^outc.*.csv", full.names=TRUE)
outc=lapply(filenames,fread)
outcome=do.call(rbind,lapply(1:length(outc), function(i) 
  select(as.data.frame(outc[i]),primaryid,outc_cod)))

filenames <- list.files(pattern="^reac.*.csv", full.names=TRUE)
reac=lapply(filenames,fread)
reaction=do.call(rbind,lapply(1:length(reac), function(i) 
  select(as.data.frame(reac[i]),primaryid,pt)))

my_database <- src_sqlite("adverse_events", create = TRUE) 

copy_to(my_database,demography,temporary = FALSE) # uploading demography data
copy_to(my_database,drug,temporary = FALSE)       # uploading drug data
copy_to(my_database,indication,temporary = FALSE) # uploading indication data
copy_to(my_database,reaction,temporary = FALSE)   # uploading reaction data
copy_to(my_database,outcome,temporary = FALSE)     #uploading outcome data

my_db <- src_sqlite("adverse_events", create = FALSE)
# create is false now because I am connecting to an existing database

src_tbls(my_db)

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

