# Uncertainty poster
source("rscripts/Uncertainty_sd.R")

library("RColorBrewer")

# plot coeff of var against mean area
cv_area_gg <- function(data){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(any(datain=="s")){
    xlabs <- "Mean Area (soil moisture)"
    ylabs <-"CV (soil moisture)"
  }
  else
  {
    xlabs <- "Mean Area (runoff)"
    ylabs <-"CV (runoff)"
  }
  save_name <- paste(paste(datain[1:3],collapse="_"),"_cv_area.png",sep="")
  print(save_name)
  ggplot(data, aes(x = mean_area, y = sd_area/mean_area, col = aft_1900)) +
    geom_point() +
    geom_smooth(se = F, span = 1) +
    labs(x=xlabs, y=ylabs, color="") +
    ylab(ylabs) +
    theme_bw()+
    scale_color_manual(labels = c("Before 1900","After 1900"),values = c("darkorange","royalblue4"))+
    theme(legend.position="top")
  
  ggsave(save_name)
}

cv_area_gg(uncer_ceu_s_all)
cv_area_gg(uncer_ceu_q_all)
cv_area_gg(uncer_med_s_all)
cv_area_gg(uncer_med_q_all)

# plot coeff of var against mean area
cv_sev_gg <- function(data){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(any(datain=="s")){
    xlabs <- "Mean Sev (soil moisture)"
    ylabs <-"CV (soil moisture)"
  }
  else
  {
    xlabs <- "Mean Sev (runoff)"
    ylabs <-"CV (runoff)"
  }
  save_name <- paste(paste(datain[1:3],collapse="_"),"_cv_sev.png",sep="")
  print(save_name)
  ggplot(data, aes(x = mean_sev, y = sd_sev/mean_sev, col = aft_1900)) +
    geom_point() +
    geom_smooth(se = F, span = 1) +
    labs(x=xlabs, y=ylabs, color="") +
    ylab(ylabs) +
    theme_bw()+
    scale_color_manual(labels = c("Before 1900","After 1900"), values = c("darkorange","royalblue4"))+
    theme(legend.position="top")
  
  ggsave(save_name)
}

cv_sev_gg(uncer_ceu_s_all)
cv_sev_gg(uncer_ceu_q_all)
cv_sev_gg(uncer_med_s_all)
cv_sev_gg(uncer_med_q_all)


# plots sd for each parameter set over time
my_cols <- colorRampPalette(c("darkred","darkorange","goldenrod","skyblue","royalblue4"))
# cols_val <- gray(seq(0.2, 0.9, length.out = 10))

sd_parts_gg_area <- function(data, reg, var){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(var=="s"){
    ylabs <-"soil moisture: standard deviation (area)"
  }
  else
  {
    ylabs <-"runoff: standard deviation (area)"
  }
  if(any(datain=="met")){
    leg.title <-"meteorological set id #"
    sw <-"met"
  }
  else{
    leg.title <-"parameter set id #"
    sw <-"par"
  }
  if(sw == "met"){
    my_fac =factor(data$met)
  }else{
    my_fac =factor(data$par)
  }
  
  save_name <- paste(paste(datain[1:2],collapse="_"),"sd",var,reg,"area.png",collapse="_")
  
  ggplot(data[reg==reg& var==var,], aes(x = yr, y = area_sd)) +
    #geom_line(aes(color=my_fac)) +
    geom_smooth(aes(color=my_fac),se = F,span = 0.3, method = "loess")+
    labs(x="Year", y=ylabs, color=leg.title,title=reg) +
    theme_bw()+
    scale_color_manual(labels = as.character(1:10), values = my_cols(10))+
    theme(legend.position="top")
  ggsave(save_name)
}
sd_parts_gg_sev <- function(data, reg, var){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(var=="s"){
    ylabs <-"soil moisture: standard deviation (severity)"
  }
  else{
    ylabs <-"runoff: standard deviation (severity)"
  }
  if(any(datain=="met")){
    leg.title <-"meteorological set id #"
    sw <-"met"
  }
  else{
    leg.title <-"parameter set id #"
    sw <-"par"
  }
  save_name <- paste(paste(datain[1:2],collapse="_"),"sd",var,reg,"sev.png",collapse="_")
  if(sw == "met"){
    my_fac =factor(data$met)
  }else{
    my_fac =factor(data$par)
  }
  
  ggplot(data[reg==reg& var==var,], aes(x = yr, y = severity_sd)) +
    #geom_line(aes(color=my_fac))+
    geom_smooth(aes(color=my_fac),se = F,span = 0.3, method = "loess")+
    labs(x="Year", y=ylabs, color=leg.title,title=reg) +
    theme_bw()+
    scale_color_manual(labels = as.character(1:10), values = my_cols(10))+
    theme(legend.position="top")
  ggsave(save_name)
}

sd_parts_gg_area(uncer_met, reg = "CEU", var= "q")
sd_parts_gg_area(uncer_met, reg = "CEU", var= "s")
sd_parts_gg_area(uncer_met, reg = "MED", var= "q")
sd_parts_gg_area(uncer_met, reg = "MED", var= "s")
sd_parts_gg_sev(uncer_met, reg = "CEU", var= "q")
sd_parts_gg_sev(uncer_met, reg = "CEU", var= "s")
sd_parts_gg_sev(uncer_met, reg = "MED", var= "q")
sd_parts_gg_sev(uncer_met, reg = "MED", var= "s")
sd_parts_gg_area(uncer_par, reg = "CEU", var= "q")
sd_parts_gg_area(uncer_par, reg = "CEU", var= "s")
sd_parts_gg_area(uncer_par, reg = "MED", var= "q")
sd_parts_gg_area(uncer_par, reg = "MED", var= "s")
sd_parts_gg_sev(uncer_par, reg = "CEU", var= "q")
sd_parts_gg_sev(uncer_par, reg = "CEU", var= "s")
sd_parts_gg_sev(uncer_par, reg = "MED", var= "q")
sd_parts_gg_sev(uncer_par, reg = "MED", var= "s")


cv_parts_gg_area <- function(data, reg, var){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(var=="s"){
    ylabs <-"soil moisture: cv (area)"
  }
  else
  {
    ylabs <-"runoff: cv (area)"
  }
  if(any(datain=="met")){
    leg.title <-"meteorological set id #"
    sw <-"met"
  }
  else{
    leg.title <-"parameter set id #"
    sw <-"par"
  }
  if(sw == "met"){
    my_fac =factor(data$met)
  }else{
    my_fac =factor(data$par)
  }
  
  save_name <- paste(paste(datain[1:2],collapse="_"),"cv",var,reg,"area.png",collapse="_")
  
  ggplot(data[reg==reg& var==var,], aes(x = yr, y = area_sd/area)) +
    #geom_line(aes(color=my_fac)) +
    geom_smooth(aes(color=my_fac),se = F,span = 0.3, method = "loess")+
    labs(x="Year", y=ylabs, color=leg.title,title=reg) +
    theme_bw()+
    scale_color_manual(labels = as.character(1:10), values = my_cols(10))+
    theme(legend.position="top")
  ggsave(save_name)
}
cv_parts_gg_sev <- function(data, reg, var){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(var=="s"){
    ylabs <-"soil moisture: cv (severity)"
  }
  else{
    ylabs <-"runoff: cv (severity)"
  }
  if(any(datain=="met")){
    leg.title <-"meteorological set id #"
    sw <-"met"
  }
  else{
    leg.title <-"parameter set id #"
    sw <-"par"
  }
  save_name <- paste(paste(datain[1:2],collapse="_"),"cv",var,reg,"sev.png",collapse="_")
  if(sw == "met"){
    my_fac =factor(data$met)
  }else{
    my_fac =factor(data$par)
  }
  
  ggplot(data[reg==reg& var==var,], aes(x = yr, y = severity_sd/severity)) +
    #geom_line(aes(color=my_fac))+
    geom_smooth(aes(color=my_fac),se = F,span = 0.3, method = "loess")+
    labs(x="Year", y=ylabs, color=leg.title,title=reg) +
    theme_bw()+
    scale_color_manual(labels = as.character(1:10), values = my_cols(10))+
    theme(legend.position="top")
  ggsave(save_name)
}

cv_parts_gg_area(uncer_met, reg = "CEU", var= "q")
cv_parts_gg_area(uncer_met, reg = "CEU", var= "s")
cv_parts_gg_area(uncer_met, reg = "MED", var= "q")
cv_parts_gg_area(uncer_met, reg = "MED", var= "s")
cv_parts_gg_sev(uncer_met, reg = "CEU", var= "q")
cv_parts_gg_sev(uncer_met, reg = "CEU", var= "s")
cv_parts_gg_sev(uncer_met, reg = "MED", var= "q")
cv_parts_gg_sev(uncer_met, reg = "MED", var= "s")
cv_parts_gg_area(uncer_par, reg = "CEU", var= "q")
cv_parts_gg_area(uncer_par, reg = "CEU", var= "s")
cv_parts_gg_area(uncer_par, reg = "MED", var= "q")
cv_parts_gg_area(uncer_par, reg = "MED", var= "s")
cv_parts_gg_sev(uncer_par, reg = "CEU", var= "q")
cv_parts_gg_sev(uncer_par, reg = "CEU", var= "s")
cv_parts_gg_sev(uncer_par, reg = "MED", var= "q")
cv_parts_gg_sev(uncer_par, reg = "MED", var= "s")

# elegant tiled heat map

library(reshape2) #melt(), dcast() for data reformatting
library(plyr) #ddply() for data reformatting
# my_cols2 <- colorRampPalette(c("darkred","darkorange","goldenrod")) # 70s return
my_cols2 <- colorRampPalette(c("blue4","skyblue"))
rank_sev_tiles_met <- function(reg_sel,var_sel){
  uncer_met$rank_sev_sd <- cut(uncer_met$rank_sev_sd,
                            breaks = c(0,50,100,150,200,250),
                            labels=c("0-50","50-100","100-150","150-200","200-250"))
  
  ggplot(uncer_met[reg == reg_sel&var==var_sel,], aes(x = yr, y = met, fill = rank_sev_sd)) +
    geom_tile() +
    scale_y_continuous(breaks = 1:10) +
    labs(x="Year", y="meteorological set", fill="rank sd(severity) ",title=paste0(reg_sel,": ", var_sel)) +
    theme_bw()+
    scale_fill_manual(values= my_cols2(5))+
    theme(legend.position="right",
          #remove plot background
          plot.background=element_blank(),
          #remove plot border
          panel.border=element_blank())
  ggsave(paste('tiles',reg_sel,var_sel,'sev_met.png',sep="_"))
  
}

rank_sev_tiles_met("MED","q")
rank_sev_tiles_met("CEU","q")
rank_sev_tiles_met("MED","s")
rank_sev_tiles_met("CEU","s")

rank_area_tiles_met <- function(reg_sel,var_sel){
  uncer_met$rank_area_sd <- cut(uncer_met$rank_area_sd,
                               breaks = c(0,50,100,150,200,250),
                               labels=c("0-50","50-100","100-150","150-200","200-250"))
  ggplot(uncer_met[reg == reg_sel&var==var_sel,], aes(x = yr, y = met, fill = rank_area_sd)) +
    geom_tile() +
    scale_y_continuous(breaks = 1:10) +
    labs(x="Year", y="meteorological set", fill="rank sd(area) ",title=paste(reg_sel,":", var_sel)) +
    theme_bw()+
    scale_fill_manual(values= my_cols2(5))+
    theme(legend.position="right",
          #remove plot background
          plot.background=element_blank(),
          #remove plot border
          panel.border=element_blank())
  ggsave(paste('tiles',reg_sel,var_sel,'area_met.png',sep="_"))
  
}

rank_area_tiles_met("MED","q")
rank_area_tiles_met("CEU","q")
rank_area_tiles_met("MED","s")
rank_area_tiles_met("CEU","s")

rank_sev_tiles_par <- function(reg_sel,var_sel){
  uncer_par$rank_sev_sd <- cut(uncer_par$rank_sev_sd,
                               breaks = c(0,50,100,150,200,250),
                               labels=c("0-50","50-100","100-150","150-200","200-250"))
  ggplot(uncer_par[reg == reg_sel&var==var_sel,], aes(x = yr, y = par, fill = rank_sev_sd)) +
    geom_tile() +
    scale_y_continuous(breaks = 1:10) +
    labs(x="Year", y="parameter set", fill="rank sd(severity) ",title=paste(reg_sel,":", var_sel)) +
    theme_bw()+
    scale_fill_manual(values= my_cols2(5))+
    theme(legend.position="right",
          #remove plot background
          plot.background=element_blank(),
          #remove plot border
          panel.border=element_blank())
  ggsave(paste('tiles',reg_sel,var_sel,'sev_par.png',sep="_"))
  
}

rank_sev_tiles_par("MED","q")
rank_sev_tiles_par("CEU","q")
rank_sev_tiles_par("MED","s")
rank_sev_tiles_par("CEU","s")

rank_area_tiles_par <- function(reg_sel,var_sel){
  uncer_par$rank_area_sd <- cut(uncer_par$rank_area_sd,
                               breaks = c(0,50,100,150,200,250),
                               labels=c("0-50","50-100","100-150","150-200","200-250"))
  ggplot(uncer_par[reg == reg_sel&var==var_sel,], aes(x = yr, y = par, fill = rank_area_sd)) +
    geom_tile() +
    scale_y_continuous(breaks = 1:10) +
    labs(x="Year", y="parameter set", fill="rank sd(area) ",title=paste(reg_sel,":", var_sel)) +
    theme_bw()+
    scale_fill_manual(values= my_cols2(5))+
    theme(legend.position="right",
          #remove plot background
          plot.background=element_blank(),
          #remove plot border
          panel.border=element_blank())
  ggsave(paste('tiles',reg_sel,var_sel,'area_par.png',sep="_"))
  
}

rank_area_tiles_par("MED","q")
rank_area_tiles_par("CEU","q")
rank_area_tiles_par("MED","s")
rank_area_tiles_par("CEU","s")


# taylor diagram
library(plotrix)
blacols <- my_cols(10)
taylor.diagram(uncer[reg == "CEU"&var=="q"&met=="1"&par=="1", area], model=uncer[reg == "CEU"&var=="q"&met=="1"&par=="2", area], pos.cor = T, pch=1, col=blacols[2])

for(i in 1:10){
  print(i)
  for(k in 1:10){
    if(k==1&i==1|i==1&k==7){
      
    }else{
      taylor.diagram(uncer[reg == "CEU"&var=="q"&met=="1"&par=="1", area], model=uncer[reg == "CEU"&var=="q"&met==as.character(k)&par==as.character(i), area], pos.cor = T,add=T,pch=k,col=blacols[i])

    }
  }
}


taylor.diagram(uncer[reg == "MED"&var=="q"&met=="1"&par=="1", area], model=uncer[reg == "MED"&var=="q"&met=="1"&par=="2", area], pos.cor = T, pch=1, col=blacols[2])
for(i in 1:10){
  print(i)
  for(k in 1:10){
    if(k==1&i==1|i==1&k==7){
      
    }else{
      taylor.diagram(uncer[reg == "MED"&var=="q"&met=="1"&par=="1", area], model=uncer[reg == "MED"&var=="q"&met==as.character(k)&par==as.character(i), area], pos.cor = T,add=T,pch=k,col=blacols[i])
      
    }
  }
}

taylor.diagram(uncer[reg == "CEU"&var=="s"&met=="1"&par=="1", area], model=uncer[reg == "CEU"&var=="s"&met=="1"&par=="2", area], pos.cor = T, pch=1, col=blacols[2])

for(i in 1:10){
  print(i)
  for(k in 1:10){
    if(k==1&i==1|i==1&k==7){
      
    }else{
      taylor.diagram(uncer[reg == "CEU"&var=="s"&met=="1"&par=="1", area], model=uncer[reg == "CEU"&var=="s"&met==as.character(k)&par==as.character(i), area], pos.cor = T,add=T,pch=k,col=blacols[i])
      
    }
  }
}


taylor.diagram(uncer[reg == "MED"&var=="s"&met=="1"&par=="1", area], model=uncer[reg == "MED"&var=="s"&met=="1"&par=="2", area], pos.cor = T, pch=1, col=blacols[2])
for(i in 1:10){
  print(i)
  for(k in 1:10){
    if(k==1&i==1|i==1&k==7){
      
    }else{
      taylor.diagram(uncer[reg == "MED"&var=="s"&met=="1"&par=="1", area], model=uncer[reg == "MED"&var=="s"&met==as.character(k)&par==as.character(i), area], pos.cor = T,add=T,pch=k,col=blacols[i])
      
    }
  }
}

###### severity

taylor.diagram(uncer[reg == "CEU"&var=="q"&met=="1"&par=="1"&yr<1900, severity], model=uncer[reg == "CEU"&var=="q"&met=="1"&par=="2"&yr<1900, severity], pos.cor = T, pch=1, col=blacols[2], normalize=T)

for(i in 1:10){
  print(i)
  for(k in 1:10){
    if(k==1&i==1|i==1&k==7){
      
    }else{
      taylor.diagram(uncer[reg == "CEU"&var=="q"&met=="1"&par=="1"&yr<1900, severity], model=uncer[reg == "CEU"&var=="q"&met==as.character(k)&par==as.character(i)&yr<1900, severity], pos.cor = T,add=T,pch=k,col=blacols[i], normalize=T)
      
    }
  }
}


taylor.diagram(uncer[reg == "MED"&var=="q"&met=="1"&par=="1", severity], model=uncer[reg == "MED"&var=="q"&met=="1"&par=="2", severity], pos.cor = T, pch=1, col=blacols[2])
for(i in 1:10){
  print(i)
  for(k in 1:10){
    if(k==1&i==1|i==1&k==7){
      
    }else{
      taylor.diagram(uncer[reg == "MED"&var=="q"&met=="1"&par=="1", severity], model=uncer[reg == "MED"&var=="q"&met==as.character(k)&par==as.character(i), severity], pos.cor = T,add=T,pch=k,col=blacols[i])
      
    }
  }
}

taylor.diagram(uncer[reg == "CEU"&var=="s"&met=="1"&par=="1", severity], model=uncer[reg == "CEU"&var=="s"&met=="1"&par=="2", severity], pos.cor = T, pch=1, col=blacols[2])

for(i in 1:10){
  print(i)
  for(k in 1:10){
    if(k==1&i==1|i==1&k==7){
      
    }else{
      taylor.diagram(uncer[reg == "CEU"&var=="s"&met=="1"&par=="1", severity], model=uncer[reg == "CEU"&var=="s"&met==as.character(k)&par==as.character(i), severity], pos.cor = T,add=T,pch=k,col=blacols[i])
      
    }
  }
}


taylor.diagram(uncer[reg == "MED"&var=="s"&met=="1"&par=="1", severity], model=uncer[reg == "MED"&var=="s"&met=="1"&par=="2", severity], pos.cor = T, pch=1, col=blacols[2], normalize=T)
for(i in 1:10){
  print(i)
  for(k in 1:10){
    if(k==1&i==1|i==1&k==7){
      
    }else{
      taylor.diagram(uncer[reg == "MED"&var=="s"&met=="1"&par=="1", severity], 
                     model=uncer[reg == "MED"&var=="s"&met==as.character(k)&par==as.character(i), severity], pos.cor = T,add=T,pch=k,col=blacols[i], normalize=T)
      
    }
  }
}


