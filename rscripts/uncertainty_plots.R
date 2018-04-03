# Uncertainty poster
source("rscripts/Uncertainty_sd.R")

if(file.exists('figs')){
  setwd('figs')
}else{
  dir.create('figs')
  setwd('figs')
}

library("RColorBrewer")
library("plotrix")
library("reshape2") #melt(), dcast() for data reformatting
library("plyr") #ddply() for data reformatting
# colours
my_cols <- colorRampPalette(c("darkred","darkorange","gold","skyblue","royalblue2","royalblue4"))
# coeffient of var
# met sets
my_cols3 <- colorRampPalette(c("darkred","maroon2","darkorange","khaki3","skyblue","royalblue2","royalblue4"))
# par sets
my_cols2 <- colorRampPalette(c("royalblue4","steelblue2","seashell2","gray55","green2","green4")) #"greenyellow",
# -
my_cols4 <- colorRampPalette(c("limegreen","forestgreen","darkgreen"))
#tiles
# par set
my_cols5 <- colorRampPalette(c("royalblue4","royalblue2","skyblue"))
# met set
my_cols6 <- colorRampPalette(c("darkred","darkorange","khaki2"))

# for before and after 1900
my_cols7 <- colorRampPalette(c("darkorange","royalblue4"))

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
    scale_color_manual(labels = c("Before 1900","After 1900"),values = my_cols7(2))+
    theme(legend.position="top", axis.text = element_text(size = 16), axis.title = element_text(size=18),title = element_text(size=18),legend.text = element_text(size=18))

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
    scale_color_manual(labels = c("Before 1900","After 1900"),values = my_cols7(2))+
    theme(legend.position="top", axis.text = element_text(size = 16), axis.title = element_text(size=18),title = element_text(size=18),legend.text = element_text(size=18))
  
  ggsave(save_name)
}

cv_sev_gg(uncer_ceu_s_all)
cv_sev_gg(uncer_ceu_q_all)
cv_sev_gg(uncer_med_s_all)
cv_sev_gg(uncer_med_q_all)


# plots sd for each parameter set over time

# cols_val <- gray(seq(0.2, 0.9, length.out = 10))

set_span <- 0.2

sd_parts_gg_area <- function(data, reg_tmp, var_tmp){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
    ylabs <-"standard deviation (area)"

  if(any(datain=="par")){
    cols <- my_cols2(10)
    leg.title <-"meteorological set id #"
  }else if (any(datain=="met")){
    cols <- my_cols3(10)
    leg.title <-"parameter set id #"
  }
  save_name <- paste(paste(datain[1:2],collapse="_"),"sd",var_tmp,reg_tmp,"area.png",sep="_")
  
  ggplot(data[reg == reg_tmp& var==var_tmp,], aes(x = yr, y = area_sd)) +
    geom_smooth(aes(colour = as.factor(par)),se = F,span = set_span, method = "loess")+
    labs(x = "Year", y = ylabs, color = leg.title, title = paste0(reg_tmp,': ',var_tmp)) +
    theme_bw()+
    coord_cartesian(ylim=c(0,11))+
    scale_color_manual(labels = as.character(1:10), values = cols)+
    theme(legend.position="top", axis.text = element_text(size = 16), axis.title = element_text(size=18),title = element_text(size=18),legend.text = element_text(size=18))
  ggsave(save_name)
}
sd_parts_gg_sev <- function(data, reg_tmp, var_tmp){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
    ylabs <-"standard deviation (severity)"
  if(any(datain=="par")){
    cols <- my_cols2(10)
    leg.title <-"meteorological set id #"
  }else if (any(datain=="met")){
    cols <- my_cols3(10)
    leg.title <-"parameter set id #"
  }
  
  save_name <- paste(paste(datain[1:2],collapse="_"),"sd",var_tmp,reg_tmp,"sev.png",sep="_")
  
  ggplot(data[reg==reg_tmp & var==var_tmp, ], aes(x = yr, y = severity_sd)) +
    geom_smooth(aes(color=as.factor(par)),se = F,span = set_span, method = "loess")+
    labs(x = "Year", y = ylabs, color = leg.title,title = paste0(reg_tmp,': ',var_tmp)) +
    theme_bw()+
    coord_cartesian(ylim=c(0.0,0.40))+
    scale_color_manual(labels = as.character(1:10), values = cols)+
    theme(legend.position="top", axis.text = element_text(size = 16), axis.title = element_text(size=18),title = element_text(size=18),legend.text = element_text(size=18))
  ggsave(save_name)
}

sd_parts_gg_area_met <- function(data, reg_tmp, var_tmp){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  ylabs <-"standard deviation (area)"
  if(any(datain=="par")){
    cols <- my_cols2(10)
    leg.title <-"meteorological set id #"
  }else if (any(datain=="met")){
    cols <- my_cols3(10)
    leg.title <-"parameter set id #"
  }
  save_name <- paste(paste(datain[1:2],collapse="_"),"sd",var_tmp,reg_tmp,"area.png",sep="_")
  
  ggplot(data[reg == reg_tmp& var==var_tmp,], aes(x = yr, y = area_sd)) +
    #geom_line(aes(color=my_fac)) +
    geom_smooth(aes(colour = as.factor(met)),se = F,span = set_span, method = "loess")+
    labs(x="Year", y = ylabs, color = leg.title, title = paste0(reg_tmp,': ',var_tmp)) +
    theme_bw()+
    coord_cartesian(ylim=c(0,11))+
    scale_color_manual(labels = as.character(1:10), values = cols)+
    theme(legend.position="top", axis.text = element_text(size = 16), axis.title = element_text(size=18),title = element_text(size=18),legend.text = element_text(size=18))
  ggsave(save_name)
}
sd_parts_gg_sev_met <- function(data, reg_tmp, var_tmp){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  ylabs <-"standard deviation (severity)"
  if(any(datain=="par")){
    cols <- my_cols2(10)
    leg.title <-"meteorological set id #"
  }else if (any(datain=="met")){
    cols <- my_cols3(10)
    leg.title <-"parameter set id #"
  }
  
  save_name <- paste(paste(datain[1:2],collapse="_"),"sd",var_tmp,reg_tmp,"sev.png",sep="_")
  
  ggplot(data[reg==reg_tmp & var==var_tmp, ], aes(x = yr, y = severity_sd)) +
    geom_smooth(aes(color=as.factor(met)),se = F,span = set_span, method = "loess")+
    labs(x = "Year", y = ylabs, color = leg.title,title = paste0(reg_tmp,': ',var_tmp)) +
    theme_bw()+
    coord_cartesian(ylim = c(0.0,0.40))+
    scale_color_manual(labels = as.character(1:10), values = cols)+
    theme(legend.position="top", axis.text = element_text(size = 16), axis.title = element_text(size=18),title = element_text(size=18),legend.text = element_text(size=18))
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
    ylabs <-"coeff. of var. (area)"
  }else{
    ylabs <-"coeff. of var. (area)"
  }
  if(any(datain=="par")){
    cols <- my_cols2(10)
    leg.title <-"meteorological set id #"
  }else if (any(datain=="met")){
    cols <- my_cols3(10)
    leg.title <-"parameter set id #"
  }
  save_name <- paste(paste(datain[1:2],collapse="_"),"cv",var_tmp,reg_tmp,"area.png",sep="_")
  
  ggplot(data[reg == reg_tmp& var==var_tmp,], aes(x = yr, y = area_sd/area)) +
    #geom_line(aes(color=my_fac)) +
    geom_smooth(aes(colour=as.factor(par)),se = F,span = set_span, method = "loess")+
    labs(x="Year", y=ylabs, color=leg.title, title=paste0(reg_tmp,': ',var_tmp)) +
    theme_bw()+
    coord_cartesian(ylim=c(0,0.9))+
    scale_color_manual(labels = as.character(1:10), values = cols)+
    theme(legend.position="top", axis.text = element_text(size = 16), axis.title = element_text(size=18),title = element_text(size=18),legend.text = element_text(size=18))+
    geom_vline(xintercept = 1900)
  ggsave(save_name)
}
cv_parts_gg_sev <- function(data, reg_tmp, var_tmp){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(var_tmp=="s"){
    ylabs <-"coeff. of var. (severity)"
  }else{
    ylabs <-"coeff. of var. (severity)"
  }
  if(any(datain=="par")){
    cols <- my_cols2(10)
    leg.title <-"meteorological set id #"
  }else if (any(datain=="met")){
    cols <- my_cols3(10)
    leg.title <-"parameter set id #"
  }
  save_name <- paste(paste(datain[1:2],collapse="_"),"cv",var_tmp,reg_tmp,"sev.png",sep="_")
  
  ggplot(data[reg==reg_tmp & var==var_tmp, ], aes(x = yr, y = severity_sd/severity)) +
    geom_smooth(aes(color=as.factor(par)),se = F,span = set_span, method = "loess") +
    labs(x="Year", y=ylabs, color=leg.title,title = paste0(reg_tmp,': ',var_tmp)) +
    theme_bw()+
    coord_cartesian(ylim=c(0,0.4))+
    scale_color_manual(labels = as.character(1:10), values = cols)+
    theme(legend.position="top", axis.text = element_text(size = 16), axis.title = element_text(size=18),title = element_text(size=18),legend.text = element_text(size=18))
  ggsave(save_name)
}

cv_parts_gg_area_met <- function(data, reg_tmp, var_tmp){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(var_tmp=="s"){
    ylabs <-"coeff. of var. (area)"
  }else{
    ylabs <-"coeff. of var. (area)"
  }
  if(any(datain=="par")){
    cols <- my_cols2(10)
    leg.title <-"meteorological set id #"
  }else if (any(datain=="met")){
    cols <- my_cols3(10)
    leg.title <-"parameter set id #"
  }
  save_name <- paste(paste(datain[1:2],collapse="_"),"cv",var_tmp,reg_tmp,"area.png",sep="_")
  
  ggplot(data[reg == reg_tmp& var==var_tmp,], aes(x = yr, y = area_sd/area)) +
    #geom_line(aes(color=my_fac)) +
    geom_smooth(aes(colour = as.factor(met)),se = F,span = set_span, method = "loess")+
    labs(x="Year", y = ylabs, color = leg.title, title  =paste0(reg_tmp,': ',var_tmp)) +
    theme_bw()+
    coord_cartesian(ylim=c(0,0.9))+
    scale_color_manual(labels = as.character(1:10), values = cols)+
    theme(legend.position="top", axis.text = element_text(size = 16), axis.title = element_text(size=18),title = element_text(size=18),legend.text = element_text(size=18))
  ggsave(save_name)
}
cv_parts_gg_sev_met <- function(data, reg_tmp, var_tmp){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(var_tmp=="s"){
    ylabs <-"coeff. of var. (severity)"
  }else{
    ylabs <-"coeff. of var. (severity)"
  }
  if(any(datain=="par")){
    cols <- my_cols2(10)
    leg.title <-"meteorological set id #"
  }else if (any(datain=="met")){
    cols <- my_cols3(10)
    leg.title <-"parameter set id #"
  }
  save_name <- paste(paste(datain[1:2],collapse="_"),"cv",var_tmp,reg_tmp,"sev.png",sep="_")
  
  ggplot(data[reg==reg_tmp & var==var_tmp, ], aes(x = yr, y = severity_sd/severity)) +
    geom_smooth(aes(color = as.factor(met)),se = F,span = set_span, method = "loess") +
    labs(x="Year", y = ylabs, color = leg.title,title = paste0(reg_tmp,': ',var_tmp)) +
    theme_bw() +
    coord_cartesian(ylim=c(0,0.4)) +
    scale_color_manual(labels = as.character(1:10), values = cols) +
    theme(legend.position="top", axis.text = element_text(size = 16), axis.title = element_text(size=18),title = element_text(size=18),legend.text = element_text(size=18))
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
rank_sev_tiles_met <- function(reg_sel,var_sel){
  uncer_met$rank_sev_sd <- cut(uncer_met$rank_sev_sd,
                            breaks = c(0,2,4,6,8,10),
                            labels = c("< 2","2 - 4","4 - 6","6 - 8","> 8"))
  
  ggplot(uncer_met[reg == reg_sel&var==var_sel,], aes(x = yr, y = par, fill = rank_sev_sd)) +
    geom_tile() +
    scale_y_continuous(breaks = 1:10) +
    labs(x="Year", y="parameter set", fill="rank sd(severity) ",title=paste0(reg_sel,": ", var_sel)) +
    theme_bw()+
    scale_fill_manual(values= my_cols5(5))+
    theme(legend.position="top",
          #remove plot background
          plot.background=element_blank(),
          #remove plot border
          panel.border=element_blank(), axis.text = element_text(size = 16), axis.title = element_text(size=18),title = element_text(size=18),legend.text = element_text(size=18))
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
    scale_fill_manual(values= my_cols5(5))+
    theme(legend.position="top",
          #remove plot background
          plot.background=element_blank(),
          #remove plot border
          panel.border=element_blank(), axis.text = element_text(size = 16), axis.title = element_text(size=18),title = element_text(size=18),legend.text = element_text(size=18))
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
    scale_fill_manual(values= my_cols6(5))+
    theme(legend.position="top",
          #remove plot background
          plot.background=element_blank(),
          #remove plot border
          panel.border=element_blank(), axis.text = element_text(size = 16), axis.title = element_text(size=18),title = element_text(size=18),legend.text = element_text(size=18))
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
    scale_fill_manual(values= my_cols6(5))+
    theme(legend.position="top",
          #remove plot background
          plot.background=element_blank(),
          #remove plot border
          panel.border=element_blank(), axis.title = element_text(size=18),title = element_text(size=18),legend.text = element_text(size=18))
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
  scale_fill_manual(values=my_cols7(2))+
  theme(legend.position="none",
        #remove plot background
        plot.background=element_blank(),
        #remove plot border
        panel.border=element_blank(), axis.text = element_text(size = 16), axis.title = element_text(size=18),title = element_text(size=18),legend.text = element_text(size=18))
ggsave(paste('barplot_sev',reg_sel,var_sel,'av_rank_par.png',sep="_"))
}

bar_area_par = function (reg_sel, var_sel){
  ggplot(uncer_par[reg == reg_sel&var==var_sel,], aes(x=met,y=av_rank_area_sd2,fill=aft_1900)) +
    geom_bar(stat = "identity", 
             position=position_dodge()) +
    geom_errorbar(aes(ymin=av_rank_area_sd2-sd_rank_area_sd2,ymax=av_rank_area_sd2+sd_rank_area_sd2), width=.2, position=position_dodge(.9))+
    labs(y="average rank sd(area)", x="meteorological set", fill="after 1900",title=paste(reg_sel,":", var_sel)) +
    theme_bw()+
    scale_y_continuous(breaks = 1:10) +
    scale_x_continuous(breaks = 1:10) +
    scale_fill_manual(values=my_cols7(2))+
    theme(legend.position="none",
          #remove plot background
          plot.background=element_blank(),
          #remove plot border
          panel.border=element_blank(), axis.text = element_text(size = 16), axis.title = element_text(size=18),title = element_text(size=18),legend.text = element_text(size=18))
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
    scale_fill_manual(values=my_cols7(2))+
    theme(legend.position="none",
          #remove plot background
          plot.background=element_blank(),
          #remove plot border
          panel.border=element_blank(), axis.text = element_text(size = 16), axis.title = element_text(size=18),title = element_text(size=18),legend.text = element_text(size=18))
  ggsave(paste('barplot_sev',reg_sel,var_sel,'av_rank_met.png',sep="_"))
}

bar_area_met = function (reg_sel, var_sel){
  ggplot(uncer_met[reg == reg_sel&var==var_sel,], aes(x=par,y=av_rank_area_sd2,fill=aft_1900)) +
    geom_bar(stat = "identity", 
             position=position_dodge()) +
    geom_errorbar(aes(ymin=av_rank_area_sd2-sd_rank_area_sd2,ymax=av_rank_area_sd2+sd_rank_area_sd2), width=.2, position=position_dodge(.9))+
    labs(y="average rank sd(area)", x="parameter set", fill="",title=paste(reg_sel,":", var_sel)) +
    theme_bw()+
    scale_y_continuous(breaks = 1:10) +
    scale_x_continuous(breaks = 1:10) +
    scale_fill_manual(values=my_cols7(2),labels=c('before 1900','after 1900'))+
    theme(legend.position="top",
          #remove plot background
          plot.background=element_blank(),
          #remove plot border
          panel.border=element_blank(), axis.text = element_text(size = 16), axis.title = element_text(size=18),title = element_text(size=18),legend.text = element_text(size=18))
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

blacols <- my_cols3(10)

# legend info
leg.txt1 <- paste("par:", as.character(1:10))
leg.txt2 <- paste("met:", as.character(1:10))
png('taylor_legend.png')
  plot(NULL)
  legend("left",leg.txt1,lty = 1,col = blacols, bty='n', ncol=5,seg.len=0.55,cex=1.2,lwd=3)
  legend("topleft",leg.txt2,pch=1:10 , bty='n', ncol=5,cex=1.2)
dev.off()

taylor_fun_sev <- function(reg_sel,var_sel){
  save_name <- paste('taylor',reg_sel,var_sel,'sev.png',sep='_')
  png(save_name)
  taylor.diagram(uncer[reg == reg_sel&var==var_sel&met=="1"&par=="1", severity]
                 , model=uncer[reg == reg_sel&var==var_sel&met=="1"&par=="2", severity]
                 , pos.cor = T, pch=1, col=blacols[2], normalize=T,main = paste0(reg_sel,": ",var_sel," severity"),cex.axis = 1.4)
  for(p in 1:10){
    print(p)
    for(m in 1:10){
      if(m==1&p==1|p==1&m==7){
        
      }else{
        taylor.diagram(uncer[reg == reg_sel&var==var_sel&met=="1"&par=="1", severity], 
                       model=uncer[reg == reg_sel&var==var_sel&met==as.character(m)&par==as.character(p), severity]
                       , pos.cor = T,add=T,pch=m,col=blacols[p], normalize=T, main='',cex.axis = 1.4)
        
      }
    }
  }
  dev.off()
}
taylor_fun_area <- function(reg_sel,var_sel){
  save_name <- paste('taylor',reg_sel,var_sel,'area.png',sep='_')
  png(save_name)
  taylor.diagram(uncer[reg == reg_sel&var==var_sel&met=="1"&par=="1", area]
                 , model=uncer[reg == reg_sel&var==var_sel&met=="1"&par=="2", area]
                 , pos.cor = T, pch=1, col=blacols[2], normalize=T,main = paste0(reg_sel,": ",var_sel, " area"),cex.axis = 1.4)
  for(p in 1:10){
    print(p)
    for(m in 1:10){
      if(m==1&p==1|p==1&m==7){
        
      }else{
        taylor.diagram(uncer[reg == reg_sel&var==var_sel&met=="1"&par=="1", area], 
                       model=uncer[reg == reg_sel&var==var_sel&met==as.character(m)&par==as.character(p), area], pos.cor = T,add=T,pch=m,col=blacols[p], normalize=T,main = '',cex.axis = 1.4)
        
      }
    }
  }
  dev.off()
}

taylor_fun_area("MED","q")
taylor_fun_area("CEU","q")
taylor_fun_area("MED","s")
taylor_fun_area("CEU","s")
taylor_fun_sev("MED","q")
taylor_fun_sev("CEU","q")
taylor_fun_sev("MED","s")
taylor_fun_sev("CEU","s")

# Comparison met and par

com_par_met_sev <- function(var_sel,reg_sel){
  aa = (uncer_met[var == var_sel&reg==reg_sel, mean(severity_sd, na.rm = T),yr]/ uncer_par[var == var_sel&reg==reg_sel, mean(severity_sd, na.rm = T), yr])
  aa[,yr:=unique(uncer_met$yr)]
  aa[,fac:=cut(aa$V1,
               breaks = c(0,0.125,0.25,0.5,2/3,1,1.5,2,4,8,100),
               labels = c("< 1:8","1:8 - 1:4","1:4 - 1:2","1:2 - 2:3","2:3 - 1:1","1:1 - 3:2","3:2 - 2:1","2:1 - 4:1","4:1 - 8:1", "> 8:1"))]
  my_ratio <- round(length(which(aa$V1>1))/length(which(aa$V1<1)),digits=2)
  ggplot(aa,aes(x=fac))+
    geom_histogram(stat='count')+
    theme_bw()+
    labs(title = bquote(frac(sigma[met],sigma[par]) ~"="~ .(my_ratio)))
  ggsave(paste0('hist_comp_',var_sel,reg_sel,'_sev.png'))
}

com_par_met_area <- function(var_sel,reg_sel){
  aa = (uncer_met[var == var_sel&reg==reg_sel, mean(area_sd, na.rm = T),yr]/ uncer_par[var == var_sel&reg==reg_sel, mean(area_sd, na.rm = T), yr])
  aa[,yr:=unique(uncer_met$yr)]
  aa[,fac:=cut(aa$V1,
               breaks = c(0,0.125,0.25,0.5,2/3,1,1.5,2,4,8,100),
               labels = c("< 1:8","1:8 - 1:4","1:4 - 1:2","1:2 - 2:3","2:3 - 1:1","1:1 - 3:2","3:2 - 2:1","2:1 - 4:1","4:1 - 8:1", "> 8:1"))]
  my_ratio <- round(length(which(aa$V1>1))/length(which(aa$V1<1)),digits=2)
  ggplot(aa,aes(x=fac))+
    geom_histogram(stat='count')+
    theme_bw()+
    labs(title = bquote(frac(sigma[met],sigma[par]) ~"="~ .(my_ratio)))
  ggsave(paste0('hist_comp_',var_sel,reg_sel,'_area.png'))
}


com_par_met_sev('q','CEU')
com_par_met_sev('s','CEU')
com_par_met_sev('q','MED')
com_par_met_sev('s','MED')
com_par_met_area('q','CEU')
com_par_met_area('s','CEU')
com_par_met_area('q','MED')
com_par_met_area('s','MED')

# histogram of everything 

uncer_met[ ,av_area_sd:=mean(area_sd, na.rm = T),by=.(reg,var,yr)]
uncer_par[ ,av_area_sd:=mean(area_sd, na.rm = T),by=.(reg,var,yr)]
uncer_met[ ,av_sev_sd:=mean(severity_sd, na.rm = T),by=.(reg,var,yr)]
uncer_par[ ,av_sev_sd:=mean(severity_sd, na.rm = T),by=.(reg,var,yr)]
          
aa_met1 <- subset(uncer_met[aft_1900==T],select=c(reg,var, yr,av_area_sd))
aa_met1[,what:="area"]
aa_met1 <- aa_met1[,unique(av_area_sd),by=.(reg, var, yr,what)]
aa_met2 <- subset(uncer_met[aft_1900==T],select=c(reg,var, yr,av_sev_sd))
aa_met2[,what:="sev"]
aa_met2 <- aa_met2[,unique(av_sev_sd),by=.(reg, var, yr,what)]
aa_met <- rbind(aa_met1,aa_met2,use.names=F)


aa_par1 <- subset(uncer_par[aft_1900==T],select=c(reg,var, yr,av_area_sd))
aa_par1[,what:="area"]
aa_par1 <- aa_par1[,unique(av_area_sd),by=.(reg, var, yr,what)]
aa_par2 <- subset(uncer_par[aft_1900==T],select=c(reg,var, yr,av_sev_sd))
aa_par2[,what:="sev"]
aa_par2 <- aa_par2[,unique(av_sev_sd),by=.(reg, var, yr,what)]
aa_par <- rbind(aa_par1,aa_par2,use.names=F)

aa <- cbind(aa_met,subset(aa_par,select=c(V1)))

names(aa)[c(5,6)] <- c("av_met","av_par")

aa[,comb:=factor(paste(reg,var,what))]

aa[,ratio:= av_met/av_par]

aa[,fac:=cut(aa$ratio,
             breaks = c(0.25,0.5,2/3,3/4,9/10,1,10/9),
             labels = c("1:4 - 1:2","1:2 - 2:3","2:3 - 3:4","3:4 - 9:10","9:10 - 1:1","1:1 - 10:9"))]
#my_ratio <- round(length(which(aa$V1>1))/length(which(aa$V1<1)),digits=2)
ggplot(aa,aes(x=fac,fill=comb))+
  geom_histogram(stat='count')+
  theme_bw()+
  scale_fill_manual(values=my_cols(8))+
  labs(x=expression(paste(sigma[met],":",sigma[par])),fill="")+
  theme(legend.position = 'top',axis.text = element_text(size = 14), axis.title = element_text(size=14),title = element_text(size=18),legend.text = element_text(size=18))
  #labs(title = bquote(frac(sigma[met],sigma[par]) ~"="~ .(my_ratio)))
ggsave(paste0('hist_comp_all.png'))

setwd('..')