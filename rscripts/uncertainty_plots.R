# Uncertainty poster
source("rscripts/Uncertainty_sd.R")

library("RColorBrewer")

library(reshape2) #melt(), dcast() for data reformatting
library(plyr) #ddply() for data reformatting


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

set_span <- 0.2

sd_parts_gg_area <- function(data, reg_tmp, var_tmp){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(var_tmp=="s"){
    ylabs <-"soil moisture: standard deviation (area)"
  }else{
    ylabs <-"runoff: standard deviation (area)"
  }
  if(any(datain=="par")){
    leg.title <-"meteorological set id #"
  }else if (any(datain=="met")){
    leg.title <-"parameter set id #"
  }
  save_name <- paste(paste(datain[1:2],collapse="_"),"sd",var_tmp,reg_tmp,"area.png",sep="_")
  
  ggplot(data[reg == reg_tmp& var==var_tmp,], aes(x = yr, y = area_sd)) +
    #geom_line(aes(color=my_fac)) +
    geom_smooth(aes(colour=as.factor(par)),se = F,span = set_span, method = "loess")+
    labs(x="Year", y=ylabs, color=leg.title, title=reg_tmp) +
    theme_bw()+
    coord_cartesian(ylim=c(2,8))+
    scale_color_manual(labels = as.character(1:10), values = my_cols(10))+
    theme(legend.position="top")
  ggsave(save_name)
}
sd_parts_gg_sev <- function(data, reg_tmp, var_tmp){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(var_tmp=="s"){
    ylabs <-"soil moisture: standard deviation (severity)"
  }else{
    ylabs <-"runoff: standard deviation (severity)"
  }
  if(any(datain=="par")){
    leg.title <-"meteorological set id #"
  }else if (any(datain=="met")){
    leg.title <-"parameter set id #"
  }
  
  save_name <- paste(paste(datain[1:2],collapse="_"),"sd",var_tmp,reg_tmp,"sev.png",sep="_")
  
  ggplot(data[reg==reg_tmp & var==var_tmp, ], aes(x = yr, y = severity_sd)) +
    geom_smooth(aes(color=as.factor(par)),se = F,span = set_span, method = "loess")+
    labs(x="Year", y=ylabs, color=leg.title,title=reg_tmp) +
    theme_bw()+
    coord_cartesian(ylim=c(0.0,0.40))+
    scale_color_manual(labels = as.character(1:10), values = my_cols(10))+
    theme(legend.position="top")
  ggsave(save_name)
}

sd_parts_gg_area_met <- function(data, reg_tmp, var_tmp){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(var_tmp=="s"){
    ylabs <-"soil moisture: standard deviation (area)"
  }else{
    ylabs <-"runoff: standard deviation (area)"
  }
  if(any(datain=="par")){
    leg.title <-"meteorological set id #"
  }else if (any(datain=="met")){
    leg.title <-"parameter set id #"
  }
  save_name <- paste(paste(datain[1:2],collapse="_"),"sd",var_tmp,reg_tmp,"area.png",sep="_")
  
  ggplot(data[reg == reg_tmp& var==var_tmp,], aes(x = yr, y = area_sd)) +
    #geom_line(aes(color=my_fac)) +
    geom_smooth(aes(colour=as.factor(met)),se = F,span = set_span, method = "loess")+
    labs(x="Year", y=ylabs, color=leg.title, title=reg_tmp) +
    theme_bw()+
    coord_cartesian(ylim=c(2,8))+
    scale_color_manual(labels = as.character(1:10), values = my_cols(10))+
    theme(legend.position="top")
  ggsave(save_name)
}
sd_parts_gg_sev_met <- function(data, reg_tmp, var_tmp){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(var_tmp=="s"){
    ylabs <-"soil moisture: standard deviation (severity)"
  }else{
    ylabs <-"runoff: standard deviation (severity)"
  }
  if(any(datain=="par")){
    leg.title <-"meteorological set id #"
  }else if (any(datain=="met")){
    leg.title <-"parameter set id #"
  }
  
  save_name <- paste(paste(datain[1:2],collapse="_"),"sd",var_tmp,reg_tmp,"sev.png",sep="_")
  
  ggplot(data[reg==reg_tmp & var==var_tmp, ], aes(x = yr, y = severity_sd)) +
    geom_smooth(aes(color=as.factor(met)),se = F,span = set_span, method = "loess")+
    labs(x="Year", y=ylabs, color=leg.title,title=reg_tmp) +
    theme_bw()+
    coord_cartesian(ylim=c(0.0,0.40))+
    scale_color_manual(labels = as.character(1:10), values = my_cols(10))+
    theme(legend.position="top")
  ggsave(save_name)
}


sd_parts_gg_area(uncer_met, "CEU", "q")
sd_parts_gg_area(uncer_met, "CEU", "s")
sd_parts_gg_area(uncer_met, "MED", "q")
sd_parts_gg_area(uncer_met, "MED","s")
sd_parts_gg_sev(uncer_met, "CEU", "q")
sd_parts_gg_sev(uncer_met, "CEU", "s")
sd_parts_gg_sev(uncer_met, "MED", "q")
sd_parts_gg_sev(uncer_met, "MED","s")

sd_parts_gg_area_met(uncer_par, "CEU", "q")
sd_parts_gg_area_met(uncer_par, "CEU", "s")
sd_parts_gg_area_met(uncer_par, "MED", "q")
sd_parts_gg_area_met(uncer_par, "MED","s")
sd_parts_gg_sev_met(uncer_par, "CEU", "q")
sd_parts_gg_sev_met(uncer_par, "CEU", "s")
sd_parts_gg_sev_met(uncer_par, "MED", "q")
sd_parts_gg_sev_met(uncer_par, "MED","s")


cv_parts_gg_area <- function(data, reg_tmp, var_tmp){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(var_tmp=="s"){
    ylabs <-"soil moisture: coeff. of var. (severity)"
  }else{
    ylabs <-"runoff: coeff. of var. (severity)"
  }
  if(any(datain=="par")){
    leg.title <-"meteorological set id #"
  }else if (any(datain=="met")){
    leg.title <-"parameter set id #"
  }
  save_name <- paste(paste(datain[1:2],collapse="_"),"cv",var_tmp,reg_tmp,"area.png",sep="_")
  
  ggplot(data[reg == reg_tmp& var==var_tmp,], aes(x = yr, y = area_sd/area)) +
    #geom_line(aes(color=my_fac)) +
    geom_smooth(aes(colour=as.factor(par)),se = F,span = set_span, method = "loess")+
    labs(x="Year", y=ylabs, color=leg.title, title=reg_tmp) +
    theme_bw()+
    coord_cartesian(ylim=c(0,0.75))+
    scale_color_manual(labels = as.character(1:10), values = my_cols(10))+
    theme(legend.position="top")
  ggsave(save_name)
}
cv_parts_gg_sev <- function(data, reg_tmp, var_tmp){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(var_tmp=="s"){
    ylabs <-"soil moisture: coeff. of var. (severity)"
  }else{
    ylabs <-"runoff: coeff. of var. (severity)"
  }
  if(any(datain=="par")){
    leg.title <-"meteorological set id #"
  }else if (any(datain=="met")){
    leg.title <-"parameter set id #"
  }
  
  save_name <- paste(paste(datain[1:2],collapse="_"),"cv",var_tmp,reg_tmp,"sev.png",sep="_")
  
  ggplot(data[reg==reg_tmp & var==var_tmp, ], aes(x = yr, y = severity_sd/severity)) +
    geom_smooth(aes(color=as.factor(par)),se = F,span = set_span, method = "loess")+
    labs(x="Year", y=ylabs, color=leg.title,title=reg_tmp) +
    theme_bw()+
    coord_cartesian(ylim=c(0,0.25))+
    scale_color_manual(labels = as.character(1:10), values = my_cols(10))+
    theme(legend.position="top")
  ggsave(save_name)
}

cv_parts_gg_area_met <- function(data, reg_tmp, var_tmp){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(var_tmp=="s"){
    ylabs <-"soil moisture: coeff. of var. (severity)"
  }else{
    ylabs <-"runoff: coeff. of var. (severity)"
  }
  if(any(datain=="par")){
    leg.title <-"meteorological set id #"
  }else if (any(datain=="met")){
    leg.title <-"parameter set id #"
  }
  save_name <- paste(paste(datain[1:2],collapse="_"),"cv",var_tmp,reg_tmp,"area.png",sep="_")
  
  ggplot(data[reg == reg_tmp& var==var_tmp,], aes(x = yr, y = area_sd/area)) +
    #geom_line(aes(color=my_fac)) +
    geom_smooth(aes(colour=as.factor(met)),se = F,span = set_span, method = "loess")+
    labs(x="Year", y=ylabs, color=leg.title, title=reg_tmp) +
    theme_bw()+
    coord_cartesian(ylim=c(0,0.75))+
    scale_color_manual(labels = as.character(1:10), values = my_cols(10))+
    theme(legend.position="top")
  ggsave(save_name)
}
cv_parts_gg_sev_met <- function(data, reg_tmp, var_tmp){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(var_tmp=="s"){
    ylabs <-"soil moisture: coeff. of var. (severity)"
  }else{
    ylabs <-"runoff: coeff. of var. (severity)"
  }
  if(any(datain=="par")){
    leg.title <-"meteorological set id #"
  }else if (any(datain=="met")){
    leg.title <-"parameter set id #"
  }
  
  save_name <- paste(paste(datain[1:2],collapse="_"),"cv",var_tmp,reg_tmp,"sev.png",sep="_")
  
  ggplot(data[reg==reg_tmp & var==var_tmp, ], aes(x = yr, y = severity_sd/severity)) +
    geom_smooth(aes(color=as.factor(met)),se = F,span = set_span, method = "loess")+
    labs(x="Year", y=ylabs, color=leg.title,title=reg_tmp) +
    theme_bw()+
    coord_cartesian(ylim=c(0,0.25))+
    scale_color_manual(labels = as.character(1:10), values = my_cols(10))+
    theme(legend.position="top")
  ggsave(save_name)
}

cv_parts_gg_area(uncer_met, "CEU", "q")
cv_parts_gg_area(uncer_met, "CEU", "s")
cv_parts_gg_area(uncer_met, "MED", "q")
cv_parts_gg_area(uncer_met, "MED","s")
cv_parts_gg_sev(uncer_met, "CEU", "q")
cv_parts_gg_sev(uncer_met, "CEU", "s")
cv_parts_gg_sev(uncer_met, "MED", "q")
cv_parts_gg_sev(uncer_met, "MED","s")

cv_parts_gg_area_met(uncer_par, "CEU", "q")
cv_parts_gg_area_met(uncer_par, "CEU", "s")
cv_parts_gg_area_met(uncer_par, "MED", "q")
cv_parts_gg_area_met(uncer_par, "MED","s")
cv_parts_gg_sev_met(uncer_par, "CEU", "q")
cv_parts_gg_sev_met(uncer_par, "CEU", "s")
cv_parts_gg_sev_met(uncer_par, "MED", "q")
cv_parts_gg_sev_met(uncer_par, "MED","s")


# elegant tiled heat map

# my_cols2 <- colorRampPalette(c("darkred","darkorange","goldenrod")) # 70s return
my_cols2 <- colorRampPalette(c("blue4","skyblue"))
rank_sev_tiles_met <- function(reg_sel,var_sel){
  uncer_met$rank_sev_sd <- cut(uncer_met$rank_sev_sd,
                            breaks = c(0,2,4,6,8,10),
                            labels = c("< 2","2 - 4","4 - 6","6 - 8","> 8"))
  
  ggplot(uncer_met[reg == reg_sel&var==var_sel,], aes(x = yr, y = par, fill = rank_sev_sd)) +
    geom_tile() +
    scale_y_continuous(breaks = 1:10) +
    labs(x="Year", y="parameter set", fill="rank sd(severity) ",title=paste0(reg_sel,": ", var_sel)) +
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
                                breaks = c(0,2,4,6,8,10),
                                labels = c("< 2","2 - 4","4 - 6","6 - 8","> 8"))
  ggplot(uncer_met[reg == reg_sel&var==var_sel,], aes(x = yr, y = par, fill = rank_area_sd)) +
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
  ggsave(paste('tiles',reg_sel,var_sel,'area_met.png',sep="_"))
  
}

rank_area_tiles_met("MED","q")
rank_area_tiles_met("CEU","q")
rank_area_tiles_met("MED","s")
rank_area_tiles_met("CEU","s")

rank_sev_tiles_par <- function(reg_sel,var_sel){
  uncer_par$rank_sev_sd <- cut(uncer_par$rank_sev_sd,
                               breaks = c(0,2,4,6,8,10),
                               labels = c("< 2","2 - 4","4 - 6","6 - 8","> 8"))
  ggplot(uncer_par[reg == reg_sel&var==var_sel,], aes(x = yr, y = met, fill = rank_sev_sd)) +
    geom_tile() +
    scale_y_continuous(breaks = 1:10) +
    labs(x="Year", y="meteorological set", fill="rank sd(severity) ",title=paste(reg_sel,":", var_sel)) +
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
                                breaks = c(0,2,4,6,8,10),
                                labels = c("< 2","2 - 4","4 - 6","6 - 8","> 8"))
  ggplot(uncer_par[reg == reg_sel&var==var_sel,], aes(x = yr, y = met, fill = rank_area_sd)) +
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
  ggsave(paste('tiles',reg_sel,var_sel,'area_par.png',sep="_"))
  
}

rank_area_tiles_par("MED","q")
rank_area_tiles_par("CEU","q")
rank_area_tiles_par("MED","s")
rank_area_tiles_par("CEU","s")


# boxplots
bar_sev_par = function (reg_sel, var_sel){
  ggplot(uncer_par[reg == reg_sel&var==var_sel,], aes(x=met,y=av_rank_sev_sd2,fill=aft_1900)) +
  geom_bar(stat = "identity", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=av_rank_sev_sd2-sd_rank_sev_sd2,ymax=av_rank_sev_sd2+sd_rank_sev_sd2), width=.2, position=position_dodge(.9))+
  labs(y="average rank sd(severity)", x="meteorological set", fill="after 1900",title=paste(reg_sel,":", var_sel)) +
  theme_bw()+
  scale_y_continuous(breaks = 1:10) +
  scale_x_continuous(breaks = 1:10) +
  scale_fill_manual(values=my_cols(2))+
  theme(legend.position="right",
        #remove plot background
        plot.background=element_blank(),
        #remove plot border
        panel.border=element_blank())
ggsave(paste('barplot_sev',reg_sel,var_sel,'av_rank_par.png',sep="_"))
}

bar_area_par = function (reg_sel, var_sel){
  ggplot(uncer_par[reg == reg_sel&var==var_sel,], aes(x=met,y=av_rank_area_sd2,fill=aft_1900)) +
    geom_bar(stat = "identity", 
             position=position_dodge()) +
    geom_errorbar(aes(ymin=av_rank_sev_sd2-sd_rank_sev_sd2,ymax=av_rank_sev_sd2+sd_rank_sev_sd2), width=.2, position=position_dodge(.9))+
    labs(y="average rank sd(area)", x="meteorological set", fill="after 1900",title=paste(reg_sel,":", var_sel)) +
    theme_bw()+
    scale_y_continuous(breaks = 1:10) +
    scale_x_continuous(breaks = 1:10) +
    scale_fill_manual(values=my_cols(2))+
    theme(legend.position="right",
          #remove plot background
          plot.background=element_blank(),
          #remove plot border
          panel.border=element_blank())
  ggsave(paste('barplot_area',reg_sel,var_sel,'av_rank_par.png',sep="_"))
}

bar_sev_met = function (reg_sel, var_sel){
  ggplot(uncer_met[reg == reg_sel&var==var_sel,], aes(x=par,y=av_rank_sev_sd2,fill=aft_1900)) +
    geom_bar(stat = "identity", 
             position=position_dodge()) +
    geom_errorbar(aes(ymin=av_rank_sev_sd2-sd_rank_sev_sd2,ymax=av_rank_sev_sd2+sd_rank_sev_sd2), width=.2, position=position_dodge(.9))+
    labs(y="average rank sd(severity)", x="parameter set", fill="after 1900",title=paste(reg_sel,":", var_sel)) +
    theme_bw()+
    scale_y_continuous(breaks = 1:10) +
    scale_x_continuous(breaks = 1:10) +
    scale_fill_manual(values=my_cols(2))+
    theme(legend.position="right",
          #remove plot background
          plot.background=element_blank(),
          #remove plot border
          panel.border=element_blank())
  ggsave(paste('barplot_sev',reg_sel,var_sel,'av_rank_met.png',sep="_"))
}

bar_area_met = function (reg_sel, var_sel){
  ggplot(uncer_met[reg == reg_sel&var==var_sel,], aes(x=par,y=av_rank_area_sd2,fill=aft_1900)) +
    geom_bar(stat = "identity", 
             position=position_dodge()) +
    geom_errorbar(aes(ymin=av_rank_sev_sd2-sd_rank_sev_sd2,ymax=av_rank_sev_sd2+sd_rank_sev_sd2), width=.2, position=position_dodge(.9))+
    labs(y="average rank sd(area)", x="parameter set", fill="after 1900",title=paste(reg_sel,":", var_sel)) +
    theme_bw()+
    scale_y_continuous(breaks = 1:10) +
    scale_x_continuous(breaks = 1:10) +
    scale_fill_manual(values=my_cols(2))+
    theme(legend.position="right",
          #remove plot background
          plot.background=element_blank(),
          #remove plot border
          panel.border=element_blank())
  ggsave(paste('barplot_area',reg_sel,var_sel,'av_rank_met.png',sep="_"))
}

bar_sev_par("MED","q")
bar_sev_par("CEU","q")
bar_sev_par("MED","s")
bar_sev_par("CEU","s")

bar_area_par("MED","q")
bar_area_par("CEU","q")
bar_area_par("MED","s")
bar_area_par("CEU","s")

bar_sev_met("MED","q")
bar_sev_met("CEU","q")
bar_sev_met("MED","s")
bar_sev_met("CEU","s")

bar_area_met("MED","q")
bar_area_met("CEU","q")
bar_area_met("MED","s")
bar_area_met("CEU","s")

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


